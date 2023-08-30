use std::{
    borrow::Cow,
    slice::from_ref,
    mem::{take, MaybeUninit},
    cmp::Ordering,
    iter::once,
    num::NonZeroUsize,
    cell::RefCell,
    rc::Rc};
use js_sys::Function;
use wasm_bindgen::{
    closure::Closure,
    JsCast};
use web_sys::{
    PointerEvent,
    MouseEvent,
    UiEvent,
    KeyboardEvent,
    Event};
use yew::{
    Component,
    Context,
    Html,
    html,
    AttrValue,
    Callback, scheduler::Shared};
use wavexp_utils::{
    R64,
    R32,
    window,
    SliceExt,
    OptionExt,
    Point,
    Take,
    default,
    AppResult,
    r64,
    AppResultUtils,
    ToAttrValue,
    now,
    SharedExt};
use crate::{
    sound::{MSecs, Secs, Beats, SoundType, Note, NoteBlock, CustomBlock},
    visual::{HintHandler, SoundVisualiser, AnyGraphEditor},
    input::{Button, Switch, GraphEditorCanvas},
    sequencer::{SoundBlock, Sequencer}, sound_internals::AudioInput};

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
    PreparePlay,
    /// emitted when the app is ready to start playing
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
    /// file was selected to be a new audio input to be added
    AudioUploaded(Event),
    /// audio source was decoded and is ready to be used
    AddInput(Shared<AudioInput>),
    /// set the playback speed of the audio source of the Custom Audio sound block
    Speed(R32),
    /// emitted when the user clicks a button to add an audio input
    StartInputAdd,
}

impl AppEvent {
    pub fn needs_layout_rerender(&self) -> bool {
        matches!(self, Self::SetTab(..)
            | Self::Select(..)
            | Self::StartPlay
            | Self::StopPlay
            | Self::SetBlockType(..)
            | Self::Remove
            | Self::Undo(..)
            | Self::Redo(..)
            | Self::AddInput(..))
    }
}

// TODO: add a notion of visible and hidden actions
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
    RemoveSoundBlock{block_id: usize, block: SoundBlock,
        prev_selected_tab: usize},
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
    /// Returns the name of the action, or `None` if an action is hidden and thus not supposed to
    /// be shown to the user.
    /// Hidden actions are those that are only significant as context for correct reconstruction of
    /// user's actions, but are not worthy of being shown in the list of actions.
    /// Such actions are dragging an editor plane, switching between tabs, etc.
    pub fn name(&self) -> Option<&'static str> {
        match self {
            Self::Start =>
                Some("Start"),
            Self::DragPlane{..} =>
                None, // "Drag Plane"
            Self::DragPoint{..} =>
                Some("Drag Block"),
            Self::DragSelection{..} =>
                Some("Drag Selection"),
            Self::SetSelection{..} =>
                Some("Set Selection"),
            Self::AddSoundBlock{..} =>
                Some("Add Sound Block"),
            Self::AddNoteBlock{..} =>
                Some("Add Note Block"),
            Self::Select{..} =>
                None, // "Open Sound Block Editor" | "Close Sound Block Editor"
            Self::SetBlockType(..) =>
                Some("Set Sound Block Type"),
            Self::SwitchTab{..} =>
                None, // "Switch Tabs",
            Self::RemoveSoundBlock{..} =>
                Some("Remove Sound Block"),
            Self::RemoveNoteBlocks(..) =>
                Some("Remove Note Blocks"),
            Self::StretchNoteBlocks{..} =>
                Some("Drag & Remove Blocks"),
            Self::SetVolume{..} =>
                Some("Set Volume"),
            Self::SetAttack{..} =>
                Some("Set Attack Time"),
            Self::SetDecay{..} =>
                Some("Set Decay Time"),
            Self::SetSustain{..} =>
                Some("Set Sustain Level"),
            Self::SetRelease{..} =>
                Some("Set Release Time"),
            Self::SetTempo{..} =>
                Some("Set Tempo"),
            Self::SetSnapStep{..} =>
                Some("Set Snap Step"),
            Self::SetMasterVolume{..} =>
                Some("Set Master Volume"),
            Self::SetRepCount{..} =>
                Some("Set Sound Block Repetition Count"),
            Self::RemoveCustomBlocks(removed) => Some(if removed.len() == 1 {
                "Remove Custom Audio Block"
            } else {
                "Remove Custom Audio Blocks"
            }),
            Self::AddCustomBlock(..) =>
                Some("Add Custom Audio Block"),
            Self::SetSpeed{..} =>
                Some("Set Custom Audio's Playback Speed")
        }
    }
}

/// carries all the app-wide settings that are passed to all the event receivers
pub struct AppContext {
    frame: Secs,
    snap_step: R64,
    selected_tab: usize,
    event_emitter: Callback<AppEvent>,
    actions: Vec<AppAction>,
    undid_actions: usize,
    action_done: bool
}

impl AppContext {
    pub fn new(event_emitter: Callback<AppEvent>) -> AppResult<Self> {
        Ok(Self{
            frame: now().to_app_result()? / 1000,
            snap_step: r64![1],
            selected_tab: default(),
            undid_actions: 0,
            actions: vec![AppAction::Start],
            action_done: false,
            event_emitter})
    }

    pub fn frame(&self) -> Secs {self.frame}
    pub fn snap_step(&self) -> R64 {self.snap_step}
    pub fn selected_tab(&self) -> usize {self.selected_tab}
    pub fn actions(&self) -> &[AppAction] {&self.actions}
    pub fn event_emitter(&self) -> &Callback<AppEvent> {&self.event_emitter}
    pub fn emit_event(&self, event: AppEvent) {self.event_emitter.emit(event)}

    pub fn register_action(&mut self, action: AppAction) {
        self.actions.drain(self.actions.len() - take(&mut self.undid_actions) ..);
        self.actions.push(action);
        self.action_done = true;
    }
}

pub struct App {
    sound_visualiser: SoundVisualiser,
    sequencer: Shared<Sequencer>,
    ctx: AppContext,
    hint_handler: HintHandler,
    frame_emitter: Function,
    selected_block: Option<usize>
}

impl Component for App {
    type Message = AppEvent;
    type Properties = ();

    fn create(ctx: &Context<Self>) -> Self {
        let cb = ctx.link().callback(AppEvent::Frame);
        let ctx = AppContext::new(ctx.link().callback(|x| x)).unwrap();
        let sound_visualiser = SoundVisualiser::new();

        let res = Self{
            selected_block: None,
            hint_handler: default(),
            sequencer: Rc::new(RefCell::new(Sequencer::new().unwrap())),
            frame_emitter: Closure::<dyn Fn(_)>::new(move |x| cb.emit(R64::new_or(r64![0], x)))
                .into_js_value().unchecked_into(),
            sound_visualiser, ctx};
        window().request_animation_frame(&res.frame_emitter).unwrap();
        res
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        let res: AppResult<!> = try {
            match msg {
                AppEvent::SnapStep(to) => {
                    self.ctx.register_action(AppAction::SetSnapStep{from: self.ctx.snap_step, to});
                    self.ctx.snap_step = to;
                }

                AppEvent::SetTab(to) => {
                    self.ctx.register_action(AppAction::SwitchTab{from: self.ctx.selected_tab, to});
                    self.ctx.selected_tab = to;
                }

                AppEvent::KeyPress(_, ref e) if &e.code() == "KeyZ" && e.meta_key() && !e.repeat() =>
                if e.shift_key() {
                    if self.ctx.undid_actions > 0 {
                        let a = unsafe{self.ctx.actions.get_unchecked(
                            self.ctx.actions.len() - self.ctx.undid_actions)};
                        self.ctx.emit_event(AppEvent::Redo(from_ref(a).to_box()));
                        self.ctx.undid_actions -= 1;
                    }
                } else if self.ctx.undid_actions < self.ctx.actions.len() - 1 {
                    self.ctx.undid_actions += 1;
                    let a = unsafe{self.ctx.actions.get_unchecked(
                        self.ctx.actions.len() - self.ctx.undid_actions)};
                    self.ctx.emit_event(AppEvent::Undo(from_ref(a).to_box()));
                }

                AppEvent::Unwind(n) => {
                    let unwound = self.ctx.actions
                        .get(self.ctx.actions.len() - n - self.ctx.undid_actions ..)
                        .to_app_result()?
                        .iter().rev().cloned().collect();
                    self.ctx.undid_actions += n;
                    self.ctx.emit_event(AppEvent::Undo(unwound))
                }

                AppEvent::Rewind(n) => {
                    let rewound = self.ctx.actions
                        .get({let x = self.ctx.actions.len() - self.ctx.undid_actions; x .. x + n})
                        .to_app_result()?
                        .to_box();
                    self.ctx.undid_actions -= n;
                    self.ctx.emit_event(AppEvent::Redo(rewound))
                }

                AppEvent::Frame(frame) => {
                    window().request_animation_frame(&self.frame_emitter)?;
                    self.ctx.frame = frame / 1000
                }

                AppEvent::Select(to) => if to != self.selected_block {
                    let prev_selected_tab = self.ctx.selected_tab.take();
                    self.ctx.register_action(AppAction::Select{
                        from: self.selected_block, to,
                        prev_selected_tab});
                    self.selected_block = to
                }

                AppEvent::Remove => {
                    let sequencer = self.sequencer.get()?;
                    let mut pattern = sequencer.pattern().get_mut()?;
                    let id = self.selected_block.take().to_app_result()?;
                    let block_id = *pattern.selection().get(id).to_app_result()?;
                    // Safety: `id` comes from `pattern.selection()`, thus it's guaranteed to be a
                    // valid index in the `pattern` itself.
                    let mut removed = MaybeUninit::uninit();
                    pattern.remove_points(once(block_id), |(_, x)| _ = removed.write(x))?;
                    let prev_selected_tab = self.ctx.selected_tab.take();
                    self.ctx.register_action(AppAction::RemoveSoundBlock{
                        block_id,
                        block: unsafe{removed.assume_init()},
                        prev_selected_tab
                    });
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
                    match *action {
                        AppAction::Select{from, prev_selected_tab, ..} => {
                            self.selected_block = from;
                            self.ctx.selected_tab = prev_selected_tab;
                        }

                        AppAction::RemoveSoundBlock{block_id, ref block, prev_selected_tab} => unsafe {
                            self.sequencer.get()?.pattern().get_mut()?
                                .insert_point(block_id, block.clone());
                            self.selected_block = Some(block_id);
                            self.ctx.selected_tab = prev_selected_tab;
                        }

                        AppAction::SwitchTab{from, ..} =>
                            self.ctx.selected_tab = from,

                        AppAction::SetSnapStep{from, ..} =>
                            self.ctx.snap_step = from,

                        _ => (),
                    }
                }

                AppEvent::Redo(ref actions) => for action in actions.iter() {
                    match *action {
                        AppAction::Select{to, ..} => {
                            self.selected_block = to;
                            self.ctx.selected_tab = 0;
                        }
                        
                        AppAction::RemoveSoundBlock{block_id, ..} => {
                            self.sequencer.get()?.pattern().get_mut()?
                                .remove_points(once(block_id), drop)?;
                            self.selected_block = None;
                            self.ctx.selected_tab = 0;
                        }

                        AppAction::SwitchTab{to, ..} =>
                            self.ctx.selected_tab = to,

                        AppAction::SetSnapStep{to, ..} =>
                            self.ctx.snap_step = to,

                        _ => (),
                    }
                }

                _ => ()
            }

            let res = msg.needs_layout_rerender();
            self.forward_event(msg)?;
            return res | self.ctx.action_done.take()
        };
        res.report();
        false
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        // TODO: add switching between selected blocks
        let sequencer = self.sequencer.get().unwrap();
        let pattern = sequencer.pattern().get().unwrap();
        let block = self.selected_block.and_then(|i| pattern.get_aware(i));
        let setter = ctx.link().callback(|x| x);

        html!{<>
            <div id="main-panel">
                <div id="ctrl-panel" class="dark-bg"
                data-main-hint="Settings"
                data-aux-hint={block.as_ref().map_or("General".into(), |x| x.to_attr_value())}>
                    <div id="hint" class="light-bg"
                    data-main-hint="Hint bar" data-aux-hint="for useful messages about the app's controls">
                        <span id="main-hint" ref={self.hint_handler.main_bar()}/>
                        <br/>
                        <span id="aux-hint" ref={self.hint_handler.aux_bar()}/>
                    </div>
                    if let Some(block) = block {
                        <div id="tab-list">
                            {block.tabs(&self.ctx)}
                        </div>
                        {block.sound.params(&self.ctx, &sequencer)}
                        <div id="general-ctrl" class="dark-bg">
                            <Button name="Back to project-wide settings"
                            setter={setter.reform(|_| AppEvent::Select(None))}>
                                <svg viewBox="0 0 100 100">
                                    <polygon points="
                                        20,60 50,20 80,60 70,60 70,80 30,80 30,60
                                    "/>
                                </svg>
                            </Button>
                            <Button name="Remove sound block"
                            setter={setter.reform(|_| AppEvent::Remove)}>
                                <svg viewBox="0 0 100 100">
                                    <polygon points="
                                        27,35 35,27 50,42 65,27 73,35 58,50
                                        73,65 65,73 50,58 35,73 27,65 42,50
                                    "/>
                                </svg>
                            </Button>
                        </div>
                    } else {
                        <div id="tab-list">
                            {sequencer.tabs(&self.ctx)}
                        </div>
                        {sequencer.params(&self.ctx)}
                    }
                </div>
                <GraphEditorCanvas<SoundBlock>
                editor={sequencer.pattern()}
                emitter={setter.clone()}/>
            </div>
            <div id="io-panel" data-main-hint="Editor plane settings">
                <div class="horizontal-menu" id="actions">
                    {for self.ctx.actions().iter().rev().enumerate().map(|(i, a)|
                        match i.cmp(&self.ctx.undid_actions) {
                            Ordering::Less if let Some(name) = a.name() => {
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
                                if let Some(name) = a.name() {
                                    <Button {name} class="selected"
                                    help={"Last action"}
                                    setter={Callback::noop()}>
                                        <p>{name}</p>
                                    </Button>
                                }
                            </>},

                            Ordering::Greater if let Some(name) = a.name() => {
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

                            _ => html!{}
                        }
                    )}
                </div>
                <div id="plane-settings" data-main-hint="Editor plane settings">
                    <Switch key="snap" name="Interval for blocks to snap to"
                    setter={setter.reform(|x: usize|
                        AppEvent::SnapStep(*[r64![0], r64![1], r64![0.5], r64![0.25], r64![0.125]]
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
                if sequencer.playing_since().is_finite() {
                    <Button name="Stop"
                    setter={setter.reform(|_| AppEvent::StopPlay)}>
                        <svg viewBox="3 0 100 103" height="100%">
                            <polygon points="25,25 35,25 35,75 25,75"/>
                            <polygon points="65,25 75,25 75,75 65,75"/>
                        </svg>
                    </Button>
                } else {
                    <Button name="Play"
                    setter={setter.reform(|_| AppEvent::PreparePlay)}>
                        <svg viewBox="3 0 100 103" height="100%">
                            <polygon points="25,25 75,50 25,75"/>
                        </svg>
                    </Button>
                }
                <canvas id="sound-visualiser" ref={self.sound_visualiser.canvas()} class="blue-border"
                data-main-hint="Sound visualiser"/>
            </div>
            // add a loading indicator
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
        self.hint_handler.handle_event(&event)?;
        let mut sequencer = self.sequencer.get_aware_mut()?;
        self.sound_visualiser.handle_event(&event, &sequencer)?;
        sequencer.handle_event(&event, &mut self.ctx)?;

        let mut pattern = sequencer.pattern().get_mut()?;
        Ok(if let Some(&id) = pattern.selection().first() {
            let mut block = unsafe{pattern.get_unchecked_mut(id)};
            let offset = block.offset;
            block.inner().handle_event(&event, &mut self.ctx, &sequencer, offset)?;
        })
    }
}
