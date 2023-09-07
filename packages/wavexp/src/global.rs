use std::{
    borrow::Cow,
    slice::from_ref,
    mem::{take, MaybeUninit},
    cmp::Ordering,
    iter::once,
    num::NonZeroUsize,
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
    Callback};
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
    ArrayExt,
    js_function, cell::Shared};
use crate::{
    sound::{MSecs, Secs, Beats, SoundType, Note, AudioInput},
    visual::{HintHandler, SoundVisualiser, AnyGraphEditor},
    input::{Button, Switch, GraphEditorCanvas, AudioInputButton},
    sequencer::{SoundBlock, Sequencer},
    img,
    sound_types::{NoteBlock, NoiseBlock, CustomBlock}};

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
    /// emitted when the user starts playing, either an audio input or the whole composition
    PreparePlay(Option<Shared<AudioInput>>),
    /// emitted when the app is ready to start playing
    StartPlay(Option<Shared<AudioInput>>),
    /// emitted when the user stops playing by clicking the `Play` button or if the audio has been
    /// played to the end.
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
    /// emitted when a pop-up window needs to be opened.
    OpenPopup(Popup),
    /// emitted when the current pop-up window needs to be closed
    ClosePopup,
    /// emitted when an audio input is selected, e.g. clicked
    SelectInput(Shared<AudioInput>),
    /// emitted when the edited audio input's name is changed.
    SetInputName(Event),
    // emitted when the edited audio input needs to be reversed.
    ReverseInput
}

/// a globally registered cancelable action
#[derive(Debug, Clone, PartialEq)]
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
    /// add a noise block to a graph editor
    AddNoiseBlock{block_id: usize, offset: R64, pitch: Note},
    /// add a custom audio block to a graph editor
    AddCustomBlock(usize, CustomBlock),
    /// select a sound block
    Select{from: Option<usize>, to: Option<usize>,
        prev_selected_tab: usize},
    /// set sound block type from the default undefined one
    SetBlockType(SoundType),
    /// switch tabs in the side editor
    SwitchTab{from: usize, to: usize},
    /// change the length of note blocks, optionally removing some
    StretchNoteBlocks{delta_x: R64, delta_y: isize, removed: Box<[(usize, NoteBlock)]>},
    /// change the length of noise blocks, optionally removing some
    StretchNoiseBlocks{delta_x: R64, delta_y: isize, removed: Box<[(usize, NoiseBlock)]>},
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
    /// remove a sound block
    RemoveSoundBlock{block_id: usize, block: SoundBlock, prev_selected_tab: usize},
    /// remove note blocks
    RemoveNoteBlocks(Box<[(usize, NoteBlock)]>),
    /// remove custom blocks
    RemoveCustomBlocks(Box<[(usize, CustomBlock)]>),
    /// remvove noise blocks
    RemoveNoiseBlocks(Box<[(usize, NoiseBlock)]>),
    /// set playback speed of the audio source of a Custom Audio sound block
    SetSpeed{from: R32, to: R32},
    /// register a new audio input
    AddInput(Shared<AudioInput>),
    /// Open a pop-up window.
    OpenPopup(Popup),
    /// Close a pop-up window.
    ClosePopup(Popup),
    /// change the selected audio input of the sound block.
    SelectInput{from: Option<Shared<AudioInput>>, to: Option<Shared<AudioInput>>},
    /// change the name of the currently edited audio input.
    SetInputName{from: Rc<str>, to: Rc<str>},
    /// reverse the currently edited audio input.
    ReverseInput
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
                None, // "Set Selection"
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
            Self::StretchNoteBlocks{removed, ..} => Some(if removed.is_empty() {
                "Stretch Note Blocks"
            } else {
                "Stretch & Remove Note Blocks"
            }),
            Self::StretchNoiseBlocks{removed, ..} => Some(if removed.is_empty() {
                "Stretch Noise Blocks"
            } else {
                "Stretch & Remove Noise Blocks"
            }),
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
            Self::RemoveNoiseBlocks(removed) => Some(if removed.len() == 1 {
                "Remove Noise Block"
            } else {
                "Remove Noise Blocks"
            }),
            Self::AddCustomBlock(..) =>
                Some("Add Custom Audio Block"),
            Self::AddNoiseBlock{..} =>
                Some("Add Noise Block"),
            Self::SetSpeed{..} =>
                Some("Set Custom Audio's Playback Speed"),
            Self::AddInput(..) =>
                Some("Add Audio Input"),
            Self::OpenPopup(_) =>
                None,
            Self::ClosePopup(_) =>
                None,
            Self::SelectInput{..} =>
                Some("Select Audio Input"),
            Self::SetInputName{..} =>
                Some("Rename Audio Input"),
            Self::ReverseInput =>
                Some("Reverse Audio Input")
        }
    }

    /// Try to incorporate `other` into `self`, returning either both of them,
    /// only `self` optionally modified, or none.
    pub fn merge(self, other: Self) -> Option<(Self, Option<Self>)> {
        match (self, other) {
            (Self::OpenPopup(_), Self::ClosePopup(_)) =>
                None,
            (Self::SwitchTab{from, ..}, Self::SwitchTab{to, ..}) if from == to =>
                None,
            (Self::SwitchTab{from, ..}, Self::SwitchTab{to, ..}) =>
                Some((Self::SwitchTab{from, to}, None)),
            (Self::Select{from, prev_selected_tab: 0, ..}, Self::Select{to, ..}) if from == to =>
                None,
            (Self::Select{from, prev_selected_tab, ..}, Self::Select{to, ..}) if from == to =>
                Some((Self::SwitchTab{from: prev_selected_tab, to: 0}, None)),
            (Self::Select{from, prev_selected_tab, ..}, Self::Select{to, ..}) =>
                Some((Self::Select{from, to, prev_selected_tab}, None)),
            (Self::DragPlane{editor_id: eid_1, offset_delta: off_1, scale_delta: s_1},
             Self::DragPlane{editor_id: eid_2, offset_delta: off_2, scale_delta: s_2})
            if eid_1 == eid_2
            && Some(off_1) == off_2.checked_neg()
            && s_1 == s_2.map(R64::recip) => 
                None,
            (Self::DragPlane{editor_id: eid_1, offset_delta: off_1, scale_delta: s_1},
             Self::DragPlane{editor_id: eid_2, offset_delta: off_2, scale_delta: s_2})
            if eid_1 == eid_2 =>
                Some((Self::DragPlane{
                    editor_id: eid_1,
                    offset_delta: off_1 + off_2,
                    scale_delta: s_1.zip(s_2, |d1, d2| d1 * d2)}, None)),
            (a, b) => Some((a, Some(b)))
        }
    }
}

/// Handles rendering of a pop-up window in the center of the screen.
#[derive(Debug, Clone, PartialEq)]
pub enum Popup {
    /// Choose the audio input for the selected sound block.
    ChooseInput,
    /// Edit the contined audio input.
    EditInput(Shared<AudioInput>)
}

impl Popup {
    pub fn render(&self, ctx: &AppContext, sequencer: &Sequencer) -> Html {
        let emitter = ctx.event_emitter();
        match self {
            Self::ChooseInput => html!{
                <div id="popup-bg">
                    <p>{"Choose audio input"}</p>
                    <Button name="Close the pop-up" class="small red-on-hover"
                    setter={emitter.reform(|_| AppEvent::ClosePopup)}>
                        <img::Cross/>
                    </Button>
                    <div class="blue-border" data-main-hint="Choose audio input">
                        <div class="dark-bg horizontal-menu-wrapper full">
                            <div class="horizontal-menu dark-bg">
                                {for sequencer.inputs().iter().map(|input| html!{
                                    <AudioInputButton class="extend-inner-button-panel"
                                    bps={sequencer.bps()} {input} {emitter}
                                    onclick={{
                                        let i = input.clone();
                                        emitter.reform(move |_| AppEvent::SelectInput(i.clone()))
                                    }}
                                    playing={sequencer.playback_ctx().played_input()
                                        .is_some_and(|cur| cur.eq(input))}
                                    name={input.get().map_or_else(|_| "".into(), |x| x.name().clone())}/>
                                })}
                                <Button name="Add audio input"
                                setter={emitter.reform(|_| AppEvent::StartInputAdd)}>
                                    <img::Plus/>
                                </Button>
                            </div>
                        </div>
                    </div>
                </div>
            },

            Self::EditInput(input_outer) => html!{
                <form id="popup-bg" method="dialog" onsubmit={emitter.reform(|_| AppEvent::ClosePopup)}>
                    <p>{"Edit audio input"}</p>
                    <Button name="Close the pop-up" class="small red-on-hover" submit=true>
                        <img::Cross/>
                    </Button>
                    <div class="dark-bg blue-border" data-main-hint="Edit audio input">
                        <div id="input-settings-panel">
                            if let Some(input) = input_outer.get().report() {
                                <div style="display: grid; grid-template-columns: repeat(3, 1fr)">
                                    if input.changes().reversed {
                                        <Button name="Playback direction: reverse" class="small"
                                        help="Click to reverse the audio input"
                                        setter={emitter.reform(|_| AppEvent::ReverseInput)}>
                                            <img::LeftArrow/>
                                        </Button>
                                    } else {
                                        <Button name="Playback direction: normal" class="small"
                                        help="Click to reverse the audio input"
                                        setter={emitter.reform(|_| AppEvent::ReverseInput)}>
                                            <img::RightArrow/>
                                        </Button>
                                    }
                                    <input type="text" value={input.name().clone()}
                                    placeholder="Enter name..." required=true
                                    class="dark-bg blue-border" data-main-hint="Audio input name"
                                    onchange={emitter.reform(AppEvent::SetInputName)}/>
                                </div>
                            } else {
                                <p style="color:red">{"Failed to access the audio input"}</p>
                            }
                        </div>
                    </div>
                </form>
            }
        }
    }

    pub fn as_edit_input(&self) -> Option<&Shared<AudioInput>> {
        if let Self::EditInput(v) = self {Some(v)} else {None}
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
    rerender_needed: bool,
    /// pop-ups are stacked on each other if one is opened from within another one.
    popups: Vec<Popup>
}

impl AppContext {
    pub fn new(event_emitter: Callback<AppEvent>) -> AppResult<Self> {
        Ok(Self{
            frame: now().to_app_result()? / 1000,
            snap_step: r64![1],
            selected_tab: default(),
            undid_actions: 0,
            actions: vec![AppAction::Start],
            rerender_needed: false,
            popups: vec![],
            event_emitter})
    }

    pub fn popup(&self) -> Option<&Popup> {self.popups.last()}
    pub fn frame(&self) -> Secs {self.frame}
    pub fn snap_step(&self) -> R64 {self.snap_step}
    pub fn selected_tab(&self) -> usize {self.selected_tab}
    pub fn actions(&self) -> &[AppAction] {&self.actions}
    pub fn event_emitter(&self) -> &Callback<AppEvent> {&self.event_emitter}
    pub fn emit_event(&self, event: AppEvent) {self.event_emitter.emit(event)}

    pub fn register_action(&mut self, action: AppAction) {
        self.actions.drain(self.actions.len() - take(&mut self.undid_actions) ..);
        self.rerender_needed = true;
        if let Some(last) = self.actions.pop() {
            let Some((first, second)) = last.merge(action) else {return};
            self.actions.push(first);
            let Some(second) = second else {return};
            self.actions.push(second);
        }
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
            sequencer: Sequencer::new().unwrap().into(),
            frame_emitter: js_function!(|x| cb.emit(R64::new_or(r64![0], x))),
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

                AppEvent::KeyPress(_, ref e) => match e.code().as_str() {
                    "KeyZ" if e.meta_key() && !e.repeat() => if e.shift_key() {
                        if self.ctx.undid_actions > 0 {
                            self.ctx.rerender_needed = true;
                            let a = unsafe{self.ctx.actions.get_unchecked(
                                self.ctx.actions.len() - self.ctx.undid_actions)};
                            self.ctx.emit_event(AppEvent::Redo(from_ref(a).to_box()));
                            self.ctx.undid_actions -= 1;
                        }
                    } else if self.ctx.undid_actions < self.ctx.actions.len() - 1 {
                        self.ctx.rerender_needed = true;
                        self.ctx.undid_actions += 1;
                        let a = unsafe{self.ctx.actions.get_unchecked(
                            self.ctx.actions.len() - self.ctx.undid_actions)};
                        self.ctx.emit_event(AppEvent::Undo(from_ref(a).to_box()));
                    }

                    "Escape" if let Some(closed) = self.ctx.popups.pop() => {
                        e.prevent_default();
                        self.ctx.register_action(AppAction::ClosePopup(closed));
                    }

                    _ => ()
                }

                AppEvent::Unwind(n) => {
                    self.ctx.rerender_needed = true;
                    let unwound = self.ctx.actions
                        .get(self.ctx.actions.len() - n - self.ctx.undid_actions ..)
                        .to_app_result()?
                        .iter().rev().cloned().collect();
                    self.ctx.undid_actions += n;
                    self.ctx.emit_event(AppEvent::Undo(unwound))
                }

                AppEvent::Rewind(n) => {
                    self.ctx.rerender_needed = true;
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

                AppEvent::OpenPopup(ref opened) => {
                    self.ctx.register_action(AppAction::OpenPopup(opened.clone()));
                    self.ctx.popups.push(opened.clone());
                }

                AppEvent::ClosePopup => {
                    let closed = self.ctx.popups.pop().to_app_result()?;
                    self.ctx.register_action(AppAction::ClosePopup(closed));
                }

                AppEvent::StartPlay(_) | AppEvent::StopPlay =>
                    self.ctx.rerender_needed = true,

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

                        AppAction::OpenPopup(_) =>
                            _ = self.ctx.popups.pop(),

                        AppAction::ClosePopup(ref popup) =>
                            self.ctx.popups.push(popup.clone()),

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

                        AppAction::OpenPopup(ref popup) =>
                            self.ctx.popups.push(popup.clone()),

                        AppAction::ClosePopup(_) =>
                            _ = self.ctx.popups.pop(),

                        _ => (),
                    }
                }

                _ => ()
            }

            self.forward_event(msg)?;
            return self.ctx.rerender_needed.take()
        };
        res.report();
        false
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        // TODO: add switching between selected blocks
        let sequencer = self.sequencer.get().unwrap();
        let pattern = sequencer.pattern().get().unwrap();
        let block = self.selected_block
            .and_then(|i| pattern.selection().get(i))
            .and_then(|i| pattern.get_aware(*i));
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
                                <img::House/>
                            </Button>
                            <Button name="Remove sound block" class="red-on-hover"
                            setter={setter.reform(|_| AppEvent::Remove)}>
                                <img::Cross/>
                            </Button>
                        </div>
                    } else {
                        <div id="tab-list">
                            {sequencer.tabs(&self.ctx)}
                        </div>
                        {sequencer.params(&self.ctx)}
                    }
                    if let Some(popup) = self.ctx.popup() {
                        {popup.render(&self.ctx, &sequencer)}
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
                if sequencer.playback_ctx().all_playing() {
                    <Button name="Stop"
                    setter={setter.reform(|_| AppEvent::StopPlay)}>
                        <img::Stop/>
                    </Button>
                } else {
                    <Button name="Play"
                    setter={setter.reform(|_| AppEvent::PreparePlay(None))}>
                        <img::Play/>
                    </Button>
                }
                <canvas id="sound-visualiser" ref={self.sound_visualiser.canvas()} class="blue-border"
                data-main-hint="Sound visualiser"/>
            </div>
            // add a loading indicator
            <div id="error-sign" hidden={true}
            data-main-hint="Error has occured" data-aux-hint="Check the console for more info">
                <img::Warning/>
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
