use std::{
    borrow::Cow,
    slice::from_ref,
    mem::{take, MaybeUninit},
    cmp::Ordering, iter::once, num::NonZeroUsize};
use js_sys::Function;
use wasm_bindgen::{
    closure::Closure,
    JsCast,
    UnwrapThrowExt};
use web_sys::{
    PointerEvent,
    MouseEvent,
    UiEvent,
    KeyboardEvent,
    Event,
    AudioBuffer,
    GainNode,
    AnalyserNode,
    AudioContext,
    AudioContextOptions,
    BaseAudioContext,
    OfflineAudioContext};
use yew::{
    Component,
    Context,
    Html,
    html,
    AttrValue,
    Callback};
use wavexp_utils::{
    R64,
    R32,
    window,
    SliceExt,
    OptionExt,
    ResultExt,
    Point,
    Take,
    default,
    AppResult,
    r64,
    r32, AppResultUtils};
use crate::{
    sound::{MSecs, Secs, Beats, SoundType, TabInfo, Sequencer, SoundBlock, Note, NoteBlock, CustomBlock},
    visual::{HintHandler, SoundVisualiser, AnyGraphEditor},
    input::{Button, Slider, Switch, GraphEditorCanvas}};

/// the all-encompassing event type for the app
#[derive(Debug, PartialEq, Clone)]
pub enum AppEvent {
    /// emitted every frame, i.e. roughly every 17 ms
    /// the field is the current time, but in milliseconds, unlike `AppContext::now`
    Frame(MSecs),
    /// emitted by the selected sound block when its visual representation is expected to
    /// have been changed
    RedrawEditorPlane,
    /// emitted when the user switches tabs in the side editor
    SetTab(usize),
    /// emitted when the viewport of the app changes its dimensions
    Resize,
    /// emitted when the user starts playing by clicking the `Play` button
    StartPlay,
    /// emitted when the user stops playing by clicking the `Play` button
    StopPlay,
    /// emitted when the user selects a sound block to edit in the side editor
    /// the contained value is index into the selected indices, not into the points directly
    Select(Option<usize>),
    /// emitted when the user deletes the selected sound block
    Remove,
    /// emitted when a `Noise` sound block's volume has been changed
    Volume(R32),
    /// emitted when a sound block's attack time has been changed
    Attack(Beats),
    /// emitted when a sound block's decay time has been changed
    Decay(Beats),
    /// emitted when a sound block's sustain level has been changed
    Sustain(R32),
    /// emitted when a sound block's release time has been changed
    Release(Beats),
    /// emitted when the global BPM has been changed
    Bpm(R64),
    /// emitted when the global volume has been changed
    MasterVolume(R32),
    /// emitted when the global editor snap step has been changed
    SnapStep(R64),
    /// emitted when the user selects the type of sound block for the selected sound block
    SetBlockType(SoundType),
    /// emitted when the user focuses an editor plane i.e. by holding left click
    /// the 1st field is the `GraphEditor::id` of the recipient
    Focus(usize, PointerEvent),
    /// emitted when the user moves the cursor across an editor plane
    /// the 1st field is the `GraphEditor::id` of the recipient
    Hover(usize, MouseEvent),
    /// emitted when the user presses any key on the keyboard
    /// the 1st field is the `GraphEditor::id` of the recipient
    KeyPress(usize, KeyboardEvent),
    /// emitted when the user releases any key on the keyboard
    /// the 1st field is the `GraphEditor::id` of the recipient
    KeyRelease(usize, KeyboardEvent),
    /// emitted when the user drags the cursor out of an editor plane
    /// the inner `usize` is the `GraphEditor::id` of the recipient
    Enter(usize, MouseEvent),
    /// emitted when the user drags the cursor out of an editor plane
    /// the inner `usize` is the `GraphEditor::id` of the recipient
    Leave(usize),
    /// emitted to set the hint for the user
    /// 1st is the main, shorter, hint, 2nd is the auxillary, longer, hint
    SetHint(Cow<'static, str>, Cow<'static, str>),
    /// similar to `SetHint` but gets the hint from an event's target
    FetchHint(UiEvent),
    /// emitted when the user cancels an action, by clicking the necessary key combination or by
    /// choosing the action to unwind to in the UI
    Undo(Box<[AppAction]>),
    /// emitted when the user cancels cancelling an action, by clicking the necessary key
    /// combination or by choosing the action to rewind to in the UI
    Redo(Box<[AppAction]>),
    /// emitted when the user unwinds action history in the UI.
    /// The contained number is the number of actions to undo.
    Unwind(usize),
    /// emitted when the user rewinds action history in the UI.
    /// The contained number is the number of actions to redo.
    Rewind(usize),
    /// set the repetition count of a sound block
    RepCount(NonZeroUsize),
    /// audio file was uploaded to a Custom Audio sound block
    AudioUploaded(Event),
    /// audio source was decoded and is ready to be used
    AudioProcessed(AudioBuffer),
    /// set the playback speed of the audio source of the Custom Audio sound block
    Speed(R32)
}

impl AppEvent {
    #[inline] pub fn needs_layout_rerender(&self) -> bool {
        matches!(self, Self::SetTab(..)
            | Self::Select(..)
            | Self::StartPlay
            | Self::StopPlay
            | Self::SetBlockType(..)
            | Self::Remove
            | Self::Undo(..)
            | Self::Redo(..))
    }
}

/// a globally registered cancelable action
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AppAction {
    /// start the session; added to the action stack by default and is always the first one
    Start,
    /// drag the plane of a graph editor
    DragPlane{editor_id: usize,
        offset_delta: Point, scale_delta: [R64; 2]},
    /// drag a point of a graph editor
    DragPoint{editor_id: usize, point_id: usize,
        delta: [R64; 2],
        meta: bool},
    /// drag selection in a graph editor
    DragSelection{editor_id: usize,
        delta: [R64; 2],
        meta: bool},
    /// change selection in a graph editor
    SetSelection{editor_id: usize,
        prev_ids: Box<[usize]>, prev_src: [R64; 2], prev_size: [R64; 2],
        cur_ids: Box<[usize]>,  cur_src: [R64; 2],  cur_size: [R64; 2]},
    /// add a sound block to the sequencer
    AddSoundBlock{block_id: usize, offset: R64, layer: i32},
    /// add a note block to a graph editor
    AddNoteBlock{block_id: usize, offset: R64, value: Note},
    /// add a custom audio block to a graph editor
    AddCustomBlock(usize, CustomBlock),
    /// select a sound block
    Select{from: Option<usize>, to: Option<usize>,
        prev_selected_tab: usize},
    /// set sound block type from the default undefined one
    SetBlockType(SoundType),
    /// switch tabs in the side editor
    SwitchTab{from: usize, to: usize},
    /// remove sound blocks
    RemoveSoundBlock(usize, SoundBlock),
    /// remove note blocks
    RemoveNoteBlocks(Box<[(usize, NoteBlock)]>),
    /// change the length of note blocks, optionally removing some
    StretchNoteBlocks{delta_x: R64, delta_y: isize, removed: Box<[(usize, NoteBlock)]>},
    /// change sound's volume
    SetVolume{from: R32, to: R32},
    /// change sound's attack time
    SetAttack{from: R64, to: R64},
    /// change sound'ss decay time
    SetDecay{from: R64, to: R64},
    /// change sound's sustain level
    SetSustain{from: R32, to: R32},
    /// change sound's release time
    SetRelease{from: R64, to: R64},
    /// change global tempo
    SetTempo{from: R64, to: R64},
    /// set global snap step for all graph editors
    SetSnapStep{from: R64, to: R64},
    /// set master gain level for the composition
    SetMasterVolume{from: R32, to: R32},
    /// set repetition count of a sound block
    SetRepCount{from: NonZeroUsize, to: NonZeroUsize},
    /// remove custom blocks
    RemoveCustomBlocks(Box<[(usize, CustomBlock)]>),
    /// set playback speed of the audio source of a Custom Audio sound block
    SetSpeed{from: R32, to: R32},
}

impl AppAction {
    #[inline] pub fn name(&self) -> &'static str {
        match self {
            Self::Start =>
                "Start",
            Self::DragPlane{..} =>
                "Drag Plane",
            Self::DragPoint{..} =>
                "Drag Block",
            Self::DragSelection{..} =>
                "Drag Selection",
            Self::SetSelection{..} =>
                "Set Selection",
            Self::AddSoundBlock{..} =>
                "Add Sound Block",
            Self::AddNoteBlock{..} =>
                "Add Note Block",
            Self::Select{to, ..} =>
                if to.is_some() {"Open Sound Block Editor"}
                else {"Close Sound Block Editor"}
            Self::SetBlockType(..) =>
                "Set Sound Block Type",
            Self::SwitchTab{..} =>
                "Switch Tabs",
            Self::RemoveSoundBlock(..) =>
                "Remove Sound Block",
            Self::RemoveNoteBlocks(..) =>
                "Remove Note Blocks",
            Self::StretchNoteBlocks{..} =>
                "Drag & Remove Blocks",
            Self::SetVolume{..} =>
                "Set Volume",
            Self::SetAttack{..} =>
                "Set Attack Time",
            Self::SetDecay{..} =>
                "Set Decay Time",
            Self::SetSustain{..} =>
                "Set Sustain Level",
            Self::SetRelease{..} =>
                "Set Release Time",
            Self::SetTempo{..} =>
                "Set Tempo",
            Self::SetSnapStep{..} =>
                "Set Snap Step",
            Self::SetMasterVolume{..} =>
                "Set Master Volume",
            Self::SetRepCount{..} =>
                "Set Sound Block Repetition Count",
            Self::RemoveCustomBlocks(removed) =>
                if removed.len() == 1 {"Remove Custom Audio Block"}
                else {"Remove Custom Audio Blocks"},
            Self::AddCustomBlock(..) =>
                "Add Custom Audio Block",
            Self::SetSpeed{..} =>
                "Set Custom Audio's Playback Speed"
        }
    }
}

pub struct PlaybackContext {
    pub audio_ctx: BaseAudioContext,
    pub gain: GainNode,
    pub analyser: AnalyserNode,
    pub started_at: Secs,
    pub now: Secs,
    pub bps: Beats
}

impl PlaybackContext {
    #[inline] fn _new(audio_ctx: BaseAudioContext, volume: R32, bps: Beats) -> AppResult<Self> {
        let analyser = audio_ctx.create_analyser()?;
        let gain = audio_ctx.create_gain()?;
        gain.gain().set_value(*volume);
        gain.connect_with_audio_node(&analyser)?
            .connect_with_audio_node(&audio_ctx.destination())?;
        Ok(Self{audio_ctx, analyser, gain, bps,
            started_at: R64::INFINITY, now: R64::INFINITY})
    }

    #[inline] pub fn new() -> AppResult<Self> {
        let audio_ctx = OfflineAudioContext::new_with_number_of_channels_and_length_and_sample_rate(
            AppContext::CHANNEL_COUNT,
            1,
            AppContext::SAMPLE_RATE as f32)?;
        Self::_new(audio_ctx.into(), r32![1.0], r64![2.0])
    }

    #[inline] pub fn volume(&self) -> R32 {
        unsafe{R32::new_unchecked(self.gain.gain().value())}
    }

    /// Prepare audio context for playing audio live
    #[inline] pub fn prepare_for_playing(&mut self) -> AppResult<()> {
        let bps = self.bps;
        let volume = self.volume();
        let audio_ctx = AudioContext::new_with_context_options(AudioContextOptions::new()
            .sample_rate(AppContext::SAMPLE_RATE as f32))?;
        Ok(*self = Self::_new(audio_ctx.into(), volume, bps)?)
    }
}

/// carries all the app-wide settings that are passed to all the event receivers
pub struct AppContext {
    playback_ctx: PlaybackContext,
    snap_step: R64,
    selected_tab: usize,
    event_emitter: Callback<AppEvent>,
    actions: Vec<AppAction>,
    undid_actions: usize,
    action_done: bool,
    playing: bool
}

impl AppContext {
    pub const SAMPLE_RATE: u32 = 44100;
    pub const CHANNEL_COUNT: u32 = 2;
    #[inline] pub fn new(event_emitter: Callback<AppEvent>) -> AppResult<Self> {
        Ok(Self{playback_ctx: PlaybackContext::new()?,
            snap_step: r64![1.0],
            selected_tab: 0,
            undid_actions: 0,
            actions: vec![AppAction::Start],
            action_done: false,
            playing: false,
            event_emitter})
    }

    #[inline] pub fn playback_ctx(&self) -> &PlaybackContext {&self.playback_ctx}
    #[inline] pub fn snap_step(&self) -> R64 {self.snap_step}
    #[inline] pub fn selected_tab(&self) -> usize {self.selected_tab}
    #[inline] pub fn actions(&self) -> &[AppAction] {&self.actions}
    #[inline] pub fn event_emitter(&self) -> &Callback<AppEvent> {&self.event_emitter}
    #[inline] pub fn emit_event(&self, event: AppEvent) {self.event_emitter.emit(event)}
    #[inline] pub fn playing(&self) -> bool {self.playing}

    #[inline] pub fn register_action(&mut self, action: AppAction) {
        self.actions.drain(self.actions.len() - take(&mut self.undid_actions) ..);
        self.actions.push(action);
        self.action_done = true;
    }

    /// takes the flag and sets it to `false`
    #[inline] pub fn recent_action_done(&mut self) -> bool {self.action_done.take()}

    #[inline] pub fn params(&self) -> Html {
        html!{
            <div id="inputs">
                <Slider key="tmp" name="Tempo"
                setter={self.event_emitter.reform(AppEvent::Bpm)}
                min={r64![30.0]} max={r64![240.0]}
                postfix="BPM"
                initial={self.playback_ctx.bps * 60}/>
                <Slider key="gain" name="Master volume"
                setter={self.event_emitter.reform(|x| AppEvent::MasterVolume(R32::from(x)))}
                initial={self.playback_ctx.volume()}/>
            </div>
        }
    }

    pub fn handle_event(&mut self, event: &AppEvent) -> AppResult<()> {
        Ok(match event {
            &AppEvent::Bpm(bpm) => {
                let to = bpm / 60;
                self.register_action(AppAction::SetTempo{from: self.playback_ctx.bps, to}); 
                self.playback_ctx.bps = to
            }

            &AppEvent::MasterVolume(to) => unsafe {
                let gain = self.playback_ctx.gain.gain();
                self.register_action(AppAction::SetMasterVolume{from: R32::new_unchecked(gain.value()), to});
                gain.set_value(*to);
            }

            AppEvent::StartPlay => {
                self.playback_ctx.prepare_for_playing()?;
                self.playing = true
            }

            AppEvent::StopPlay => {
                self.playback_ctx.started_at = Secs::INFINITY;
                self.playing = false;
            }

            &AppEvent::Frame(now) => {
                self.playback_ctx.now = now / 1000u16;
                if self.playing && self.playback_ctx.started_at.is_infinite() {
                    self.playback_ctx.started_at = self.playback_ctx.now;
                }
            }

            &AppEvent::SnapStep(to) => {
                self.register_action(AppAction::SetSnapStep{from: self.snap_step, to});
                self.snap_step = to;
            }

            AppEvent::Select(_) =>
                self.selected_tab = 0,

            &AppEvent::SetTab(to) => {
                self.register_action(AppAction::SwitchTab{from: self.selected_tab, to});
                self.selected_tab = to;
            }

            AppEvent::KeyPress(_, e) if &e.code() == "KeyZ" && e.meta_key() && !e.repeat() =>
            if e.shift_key() {
                if self.undid_actions > 0 {
                    let a = unsafe{self.actions.get_unchecked(self.actions.len() - self.undid_actions)};
                    self.emit_event(AppEvent::Redo(from_ref(a).to_vec().into()));
                    self.undid_actions -= 1;
                }
            } else if self.undid_actions < self.actions.len() - 1 {
                self.undid_actions += 1;
                let a = unsafe{self.actions.get_unchecked(self.actions.len() - self.undid_actions)};
                self.emit_event(AppEvent::Undo(from_ref(a).to_vec().into()));
            }

            &AppEvent::Unwind(n) => {
                let unwound = self.actions.get(self.actions.len() - n - self.undid_actions ..)
                    .to_app_result()?
                    .iter().rev().cloned().collect();
                self.undid_actions += n;
                self.event_emitter.emit(AppEvent::Undo(unwound))
            }

            &AppEvent::Rewind(n) => {
                let len = self.actions.len();
                let rewound = self.actions.get({let x = len - self.undid_actions; x .. x + n})
                    .to_app_result()?.to_box();
                self.undid_actions -= n;
                self.event_emitter.emit(AppEvent::Redo(rewound))
            }

            AppEvent::Undo(actions) => for action in actions.iter() {
                match *action {
                    AppAction::SwitchTab{from, ..} =>
                        self.selected_tab = from,

                    AppAction::Select{prev_selected_tab, ..} =>
                        self.selected_tab = prev_selected_tab,

                    AppAction::SetTempo{from, ..} =>
                        self.playback_ctx.bps = from,

                    AppAction::SetSnapStep{from, ..} =>
                        self.snap_step = from,

                    AppAction::SetMasterVolume{from, ..} =>
                        self.playback_ctx.gain.gain().set_value(*from),

                    _ => ()
                }
            }

            AppEvent::Redo(actions) => for action in actions.iter() {
                match *action {
                    AppAction::SwitchTab{to, ..} =>
                        self.selected_tab = to,

                    AppAction::Select{..} =>
                        self.selected_tab = 0,

                    AppAction::SetTempo{to, ..} =>
                        self.playback_ctx.bps = to,

                    AppAction::SetSnapStep{to, ..} =>
                        self.snap_step = to,

                    AppAction::SetMasterVolume{to, ..} =>
                        self.playback_ctx.gain.gain().set_value(*to),

                    _ => ()
                }
            }

            _ => ()
        })
    }
}

pub struct App {
    sound_visualiser: SoundVisualiser,
    sequencer: Sequencer,
    ctx: AppContext,
    hint_handler: HintHandler,
    frame_emitter: Function,
    selected_id: Option<usize>
}

impl Component for App {
    type Message = AppEvent;
    type Properties = ();

    fn create(ctx: &Context<Self>) -> Self {
        let cb = ctx.link().callback(AppEvent::Frame);
        let ctx = AppContext::new(ctx.link().callback(|x| x)).unwrap_throw();
        let sound_visualiser = SoundVisualiser::new();

        let res = Self{selected_id: None,
            hint_handler: default(),
            sequencer: default(),
            frame_emitter: Closure::<dyn Fn(f64)>::new(move |x| cb.emit(R64::new_or(r64![0.0], x)))
                .into_js_value().unchecked_into(),
            sound_visualiser, ctx};
        window().request_animation_frame(&res.frame_emitter).unwrap_throw();
        res
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        let res: AppResult<!> = try {
            match msg {
                AppEvent::Frame(_) =>
                    _ = window().request_animation_frame(&self.frame_emitter)?,

                AppEvent::Select(id) => {
                    if id != self.selected_id {
                        self.ctx.register_action(AppAction::Select{
                            from: self.selected_id, to: id,
                            prev_selected_tab: self.ctx.selected_tab});
                    }
                    self.selected_id = id
                }

                AppEvent::Remove => {
                    let mut pattern = self.sequencer.pattern().try_borrow_mut().to_app_result()?;
                    let id = self.selected_id.take().to_app_result()?;
                    let id = *unsafe{pattern.selection().get_unchecked(id)};
                    let mut removed = MaybeUninit::uninit();
                    pattern.remove_points(once(id), |(_, x)| _ = removed.write(x))?;
                    self.ctx.register_action(AppAction::RemoveSoundBlock(id, unsafe{removed.assume_init()}))
                }

                AppEvent::Enter(id, _) => {
                    let window = window();
                    let cb = ctx.link().callback(move |e| AppEvent::KeyPress(id, e));
                    let cb = Closure::<dyn Fn(KeyboardEvent)>::new(move |e| cb.emit(e))
                        .into_js_value().unchecked_into();
                    window.set_onkeydown(Some(&cb));
                    let cb = ctx.link().callback(move |e| AppEvent::KeyRelease(id, e));
                    let cb = Closure::<dyn Fn(KeyboardEvent)>::new(move |e| cb.emit(e))
                        .into_js_value().unchecked_into();
                    window.set_onkeyup(Some(&cb))
                }

                AppEvent::Leave(_) => {
                    let window = window();
                    let cb = ctx.link().callback(|e| AppEvent::KeyPress(AnyGraphEditor::INVALID_ID, e));
                    let cb = Closure::<dyn Fn(KeyboardEvent)>::new(move |e| cb.emit(e))
                        .into_js_value().unchecked_into();
                    window.set_onkeydown(Some(&cb));
                    let cb = ctx.link().callback(|e| AppEvent::KeyRelease(AnyGraphEditor::INVALID_ID, e));
                    let cb = Closure::<dyn Fn(KeyboardEvent)>::new(move |e| cb.emit(e))
                        .into_js_value().unchecked_into();
                    window.set_onkeyup(Some(&cb))
                }

                AppEvent::Undo(ref actions) => for action in actions.iter() {
                    if let &AppAction::Select{from, ..} = action {
                        self.selected_id = from;
                    }
                }

                AppEvent::Redo(ref actions) => for action in actions.iter() {
                    if let &AppAction::Select{to, ..} = action {
                        self.selected_id = to;
                    }
                }

                _ => ()
            }

            let res = msg.needs_layout_rerender();
            self.forward_event(msg)?;
            return res | self.ctx.recent_action_done()
        };
        res.report();
        false
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        // TODO: add switching between selected blocks
        let pattern = self.sequencer.pattern().try_borrow().unwrap_throw();
        let block = self.selected_id.map(|i| unsafe{pattern.get_unchecked_aware(i)});

        let setter = ctx.link().callback(|x| x);
        let render_tab_info = |info: &TabInfo, tab_id: usize, desc: AttrValue| -> Html {
            html!{
                <div id={(self.ctx.selected_tab == tab_id).then_some("selected-tab")}
                onpointerup={ctx.link().callback(move |_| AppEvent::SetTab(tab_id))}
                data-main-hint={info.name} data-aux-hint={desc}>
                    <p>{info.name}</p>
                </div>
            }
        };

        html!{<>
            <div id="main-panel">
                <div id="ctrl-panel" class="dark-bg"
                data-main-hint="Settings" data-aux-hint={block.as_ref().map_or(AttrValue::from("General"), |x| AttrValue::from(x.desc()))}>
                    <div id="hint" class="light-bg"
                    data-main-hint="Hint bar" data-aux-hint="for useful messages about the app's controls">
                        <span id="main-hint" ref={self.hint_handler.main_bar()}/>
                        <br/>
                        <span id="aux-hint" ref={self.hint_handler.aux_bar()}/>
                    </div>
                    if let Some((tab_aux_hint, block)) = block.map(|x| (AttrValue::from(x.desc() + ": Settings tab"), x)) {
                        <div id="tab-list">
                            {for block.sound.tabs().iter().enumerate()
                                .map(|(tab_id, tab)| render_tab_info(tab, tab_id, tab_aux_hint.clone()))}
                        </div>
                        {block.sound.params(&self.ctx, setter.clone())}
                        <div id="general-ctrl" class="dark-bg">
                            <Button name="Back to project-wide settings"
                            setter={setter.reform(|_| AppEvent::Select(None))}>
                                <svg viewBox="0 0 100 100">
                                    <polygon points="20,60 50,20 80,60 70,60 70,80 30,80 30,60"/>
                                </svg>
                            </Button>
                            <Button name="Remove sound block"
                            setter={setter.reform(|_| AppEvent::Remove)}>
                                <svg viewBox="0 0 100 100">
                                    <polygon points="27,35 35,27 50,42 65,27 73,35 58,50 73,65 65,73 50,58 35,73 27,65 42,50"/>
                                </svg>
                            </Button>
                        </div>
                    } else {
                        {self.ctx.params()}
                    }
                </div>
                <GraphEditorCanvas<SoundBlock>
                editor={self.sequencer.pattern()}
                emitter={setter.clone()}/>
            </div>
            <div id="io-panel" data-main-hint="Editor plane settings">
                <div class="horizontal-menu" id="actions">
                    {for self.ctx.actions().iter().rev().enumerate().map(|(i, a)| (a.name(), i)).map(|(name, i)|
                        match i.cmp(&self.ctx.undid_actions) {
                            Ordering::Less => {
                                let i = self.ctx.undid_actions - i;
                                html!{
                                    <Button {name} class="undone"
                                    help={match i {
                                        1 => AttrValue::Static("Click to redo this action"),
                                        2 => AttrValue::Static("Click to redo this and the previous action"),
                                        _ => format!("Click to redo this and {i} previous actions").into()
                                    }}
                                    setter={setter.reform(move |_| AppEvent::Rewind(i))}>
                                        <s>{name}</s>
                                    </Button>
                                }
                            },

                            Ordering::Equal => html!{<>
                                <div data-main-hint="Present"
                                data-aux-hint="Below this are done actions, above - undone actions">
                                    <p>{"Present"}</p>
                                </div>
                                <Button {name}
                                help={"Last action"}
                                setter={Callback::noop()}>
                                    <p>{name}</p>
                                </Button>
                            </>},

                            Ordering::Greater => {
                                let i = i - self.ctx.undid_actions;
                                html!{
                                    <Button {name}
                                    help={match i {
                                        1 => AttrValue::Static("Click to undo the next action"),
                                        _ => format!("Click to undo {i} subsequent actions").into()
                                    }}
                                    setter={setter.reform(move |_| AppEvent::Unwind(i))}>
                                        <p>{name}</p>
                                    </Button>
                                }
                            }
                        }
                    )}
                </div>
                <div id="plane-settings" data-main-hint="Editor plane settings">
                    <Switch key="snap" name="Interval for blocks to snap to"
                    setter={setter.reform(|x: usize|
                        AppEvent::SnapStep(*[r64![0.0], r64![1.0], r64![0.5], r64![0.25], r64![0.125]]
                            .get_wrapping(x)))}
                    options={vec!["None", "1", "1/2", "1/4", "1/8"]}
                    initial={match *self.ctx.snap_step {
                        x if x == 1.0   => 1,
                        x if x == 0.5   => 2,
                        x if x == 0.25  => 3,
                        x if x == 0.125 => 4,
                        _ => 0
                    }}/>
                </div>
                if self.ctx.playing() {
                    <Button name="Stop"
                    setter={setter.reform(|_| AppEvent::StopPlay)}>
                        <svg viewBox="3 0 100 103" height="100%">
                            <polygon points="25,25 35,25 35,75 25,75"/>
                            <polygon points="65,25 75,25 75,75 65,75"/>
                        </svg>
                    </Button>
                } else {
                    <Button name="Play"
                    setter={setter.reform(|_| AppEvent::StartPlay)}>
                        <svg viewBox="3 0 100 103" height="100%">
                            <polygon points="25,25 75,50 25,75"/>
                        </svg>
                    </Button>
                }
                <canvas id="sound-visualiser" ref={self.sound_visualiser.canvas()} class="blue-border"
                data-main-hint="Sound visualiser"/>
            </div>
            <div id="error-sign" hidden={true}
            data-main-hint="Error has occured" data-aux-hint="Check the console for more info">
                <svg viewBox="0 0 100 100">
                    <polygon points="10,90 50,10 90,90"/>
                    <polygon points="48,40 52,40 52,60 48,60"/>
                    <polygon points="48,70 52,70 52,74 48,74"/>
                </svg>
            </div>
        </>}
    }

    fn rendered(&mut self, ctx: &Context<Self>, first_render: bool) {
        if !first_render {return}
        let window = window();

        let cb = ctx.link().callback(|_| AppEvent::Resize);
        let cb = Closure::<dyn Fn()>::new(move || cb.emit(()))
            .into_js_value().unchecked_into();
        window.set_onresize(Some(&cb));

        let cb = ctx.link().callback(AppEvent::FetchHint);
        let cb = Closure::<dyn Fn(UiEvent)>::new(move |e| cb.emit(e))
            .into_js_value().unchecked_into();
        window.set_onpointerover(Some(&cb));

        let cb = ctx.link().callback(|e| AppEvent::KeyPress(AnyGraphEditor::INVALID_ID, e));
        let cb = Closure::<dyn Fn(KeyboardEvent)>::new(move |e| cb.emit(e))
            .into_js_value().unchecked_into();
        window.set_onkeydown(Some(&cb));

        let cb = ctx.link().callback(|e| AppEvent::KeyRelease(AnyGraphEditor::INVALID_ID, e));
        let cb = Closure::<dyn Fn(KeyboardEvent)>::new(move |e| cb.emit(e))
            .into_js_value().unchecked_into();
        window.set_onkeyup(Some(&cb));

        ctx.link().send_message(AppEvent::Resize);
    }
}

impl App {
    fn forward_event(&mut self, event: AppEvent) -> AppResult<()> {
        self.ctx.handle_event(&event)?;
        self.hint_handler.handle_event(&event, &self.ctx)?;
        self.sound_visualiser.handle_event(&event, &self.ctx)?;
        self.sequencer.handle_event(&event, &mut self.ctx)?;

        let mut pattern = self.sequencer.pattern().try_borrow_mut()?;
        Ok(if let Some(&id) = pattern.selection().first() {
            let mut block = unsafe{pattern.get_unchecked_mut(id)};
            let offset = block.offset;
            block.inner().handle_event(&event, &mut self.ctx, offset)?;
        })
    }
}
