//! This module contains the types used for communication between components of the app

use std::{any::Any, borrow::Cow, mem::transmute, num::NonZeroUsize, ops::Deref, rc::Rc};

use crate::{
    app::AppContext,
    editor::EditorContext,
    popup::Popup,
    sound::{AudioInput, Beats, MSecs, SoundType},
    visual::SpecialAction,
};
use wavexp_utils::{
    cell::Shared,
    error::{AppError, Result},
    ext::ArrayExt,
    Point, R32, R64,
};
use web_sys::{AudioBuffer, Event, KeyboardEvent, MouseEvent, PointerEvent, UiEvent};

/// the all-encompassing event type for the app
#[derive(Debug, Clone)]
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
    Undo(Box<[EditorAction]>),
    /// emitted when the user cancels cancelling an action, by clicking the necessary key
    /// combination or by choosing the action to rewind to in the UI
    Redo(Box<[EditorAction]>),
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
    /// emitted when the edited audio input needs to be reversed.
    ReverseInput,
    /// set the starting cut-off of the edited audio input.
    SetStartCutOff(Beats),
    /// set the ending cut-off of the edited audio input.
    SetEndCutOff(Beats),
    /// set the special action for editor spaces.
    SetSpecialAction(SpecialAction),
    /// prepare export of the project under the provided file name.
    PrepareExport(Rc<str>),
    /// export audio under the provided file name.
    Export(Rc<str>, AudioBuffer),
    /// set the filename under which the project will be saved
    SetOutputFileName(Event),
    /// display an explanation for why the export file name is invalid.
    ExplainInvalidExportFileName(Event),
}

/// For `AppAction::RemovePoint`
#[derive(Debug, Clone)]
pub struct RemovedPoint {
    pub point: Rc<dyn Any>,
    pub index: usize,
    pub was_selected: bool,
}

/// a globally registered cancelable action
#[derive(Debug, Clone)]
pub enum EditorAction {
    /// start the session; added to the action stack by default and is always the first one
    Start,
    /// drag the plane of a graph editor
    DragPlane {
        editor_id: usize,
        offset_delta: Point,
        scale_delta: [R64; 2],
    },
    /// drag a point of a graph editor
    DragPoint {
        editor_id: usize,
        point_id: usize,
        delta: [R64; 2],
    },
    /// drag selection in a graph editor
    DragSelection { editor_id: usize, delta: [R64; 2] },
    /// change selection in a graph editor
    SetSelection {
        editor_id: usize,
        prev_ids: Box<[usize]>,
        prev_src: [R64; 2],
        prev_size: [R64; 2],
        cur_ids: Box<[usize]>,
        cur_src: [R64; 2],
        cur_size: [R64; 2],
    },
    /// select a sound block
    Select {
        from: Option<usize>,
        to: Option<usize>,
        prev_selected_tab: usize,
    },
    /// set sound block type from the default undefined one
    SetBlockType(SoundType),
    /// switch tabs in the side editor
    SwitchTab { from: usize, to: usize },
    /// change sound's volume
    SetVolume { from: R32, to: R32 },
    /// change sound's attack time
    SetAttack { from: R64, to: R64 },
    /// change sound'ss decay time
    SetDecay { from: R64, to: R64 },
    /// change sound's sustain level
    SetSustain { from: R32, to: R32 },
    /// change sound's release time
    SetRelease { from: R64, to: R64 },
    /// change global tempo
    SetTempo { from: R64, to: R64 },
    /// set global snap step for all graph editors
    SetSnapStep { from: R64, to: R64 },
    /// set master gain level for the composition
    SetMasterVolume { from: R32, to: R32 },
    /// set repetition count of a sound block
    SetRepCount {
        from: NonZeroUsize,
        to: NonZeroUsize,
    },
    /// set playback speed of the audio source of a Custom Audio sound block
    SetSpeed { from: R32, to: R32 },
    /// register a new audio input
    AddInput(Shared<AudioInput>),
    /// Open a pop-up window.
    OpenPopup(Popup),
    /// Close a pop-up window.
    ClosePopup(Popup),
    /// change the selected audio input of the sound block.
    SelectInput {
        from: Option<Shared<AudioInput>>,
        to: Option<Shared<AudioInput>>,
    },
    /// change the name of the currently edited audio input.
    SetInputName { from: Rc<str>, to: Rc<str> },
    /// add a point onto a graph editor.
    AddPoint {
        editor_id: usize,
        point_id: usize,
        point_loc: [R64; 2],
    },
    /// remove a point from a graph editor.
    RemovePoint(usize, Box<[RemovedPoint]>),
    /// reverse the currently edited audio input.
    ReverseInput,
    /// set the currently edited audio input's starting cut off.
    SetStartCutOff { from: Beats, to: Beats },
    /// set the currently edited audio input's ending cut off.
    SetEndCutOff { from: Beats, to: Beats },
    /// change the filename under which to save the project.
    SetOutputFileName { from: Rc<str>, to: Rc<str> },
}

impl EditorAction {
    /// Returns the name of the action, or `None` if an action is hidden and thus not supposed to
    /// be shown to the user.
    /// Hidden actions are those that are only significant as context for correct reconstruction of
    /// user's actions, but are not worthy of being shown in the list of actions.
    /// Such actions are dragging an editor plane, switching between tabs, etc.
    pub fn name(&self) -> Option<&'static str> {
        match self {
            Self::Start => Some("Start"),
            Self::DragPlane { .. } => None, // "Drag Plane"
            Self::DragPoint { .. } => Some("Drag Block"),
            Self::DragSelection { .. } => Some("Drag Selection"),
            Self::SetSelection { .. } => None, // "Set Selection"
            Self::Select { .. } => None, // "Open Sound Block Editor" | "Close Sound Block Editor"
            Self::SetBlockType(..) => Some("Set Sound Block Type"),
            Self::SwitchTab { .. } => None, // "Switch Tabs",
            Self::SetVolume { .. } => Some("Set Volume"),
            Self::SetAttack { .. } => Some("Set Attack Time"),
            Self::SetDecay { .. } => Some("Set Decay Time"),
            Self::SetSustain { .. } => Some("Set Sustain Level"),
            Self::SetRelease { .. } => Some("Set Release Time"),
            Self::SetTempo { .. } => Some("Set Tempo"),
            Self::SetSnapStep { .. } => Some("Set Snap Step"),
            Self::SetMasterVolume { .. } => Some("Set Master Volume"),
            Self::SetRepCount { .. } => Some("Set Sound Block Repetition Count"),
            Self::SetSpeed { .. } => Some("Set Custom Audio's Playback Speed"),
            Self::AddInput(..) => Some("Add Audio Input"),
            Self::OpenPopup(_) => None,
            Self::ClosePopup(_) => None,
            Self::SelectInput { .. } => Some("Select Audio Input"),
            Self::SetInputName { .. } => Some("Rename Audio Input"),
            Self::AddPoint { .. } => Some("Add a point to an editor plane"),
            Self::RemovePoint(_, points) => Some(if points.len() == 1 {
                "Remove a point from an editor plane"
            } else {
                "Remove points from an editor plane"
            }),
            Self::ReverseInput => Some("Reverse Audio Input"),
            Self::SetStartCutOff { .. } => Some("Set Starting Cut-Off"),
            Self::SetEndCutOff { .. } => Some("Set Ending Cut-Off"),
            Self::SetOutputFileName { .. } => None,
        }
    }

    /// Try to incorporate `other` into `self`, returning either both of them,
    /// only `self` optionally modified, or none.
    #[allow(clippy::result_large_err)]
    #[allow(clippy::type_complexity)]
    pub fn merge(
        self,
        other: Self,
    ) -> Result<Option<(Self, Option<Self>)>, (Self, Self, AppError)> {
        Ok(match (self, other) {
            (Self::OpenPopup(_), Self::ClosePopup(_)) => None,
            (Self::SwitchTab { from, .. }, Self::SwitchTab { to, .. }) if from == to => None,
            (Self::SwitchTab { from, .. }, Self::SwitchTab { to, .. }) => {
                Some((Self::SwitchTab { from, to }, None))
            }
            (
                Self::Select {
                    from,
                    prev_selected_tab: 0,
                    ..
                },
                Self::Select { to, .. },
            ) if from == to => None,
            (
                Self::Select {
                    from,
                    prev_selected_tab,
                    ..
                },
                Self::Select { to, .. },
            ) if from == to => Some((
                Self::SwitchTab {
                    from: prev_selected_tab,
                    to: 0,
                },
                None,
            )),
            (
                Self::Select {
                    from,
                    prev_selected_tab,
                    ..
                },
                Self::Select { to, .. },
            ) => Some((
                Self::Select {
                    from,
                    to,
                    prev_selected_tab,
                },
                None,
            )),
            (
                Self::DragPlane {
                    editor_id: eid_1,
                    offset_delta: off_1,
                    scale_delta: s_1,
                },
                Self::DragPlane {
                    editor_id: eid_2,
                    offset_delta: off_2,
                    scale_delta: s_2,
                },
            ) if eid_1 == eid_2 && Some(off_1) == -off_2 && s_1 == s_2.map(R64::recip) => None,
            (
                ref a @ Self::DragPlane {
                    editor_id: eid_1,
                    offset_delta: off_1,
                    scale_delta: s_1,
                },
                ref b @ Self::DragPlane {
                    editor_id: eid_2,
                    offset_delta: off_2,
                    scale_delta: s_2,
                },
            ) if eid_1 == eid_2 => Some((
                Self::DragPlane {
                    editor_id: eid_1,
                    offset_delta: (off_1 + off_2)
                        .ok_or_else(|| (a.clone(), b.clone(), AppError::on_none()))?,
                    scale_delta: s_1.zip(s_2, |d1, d2| d1 * d2),
                },
                None,
            )),
            (a, b) => Some((a, Some(b))),
        })
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct ContextRef<'app, 'editor> {
    pub editor: &'editor EditorContext,
    pub app: &'app AppContext,
}

#[repr(C)]
pub struct ContextMut<'app, 'editor> {
    pub editor: &'editor mut EditorContext,
    pub app: &'app mut AppContext,
}

impl<'app, 'editor> Deref for ContextRef<'app, 'editor> {
    type Target = ContextMut<'app, 'editor>;
    fn deref(&self) -> &Self::Target {
        // Safety:
        // - &&T is the same as &&mut T, that `mut` is neutralised by being behind a shared ref
        // - `ContextMut` & `ContextRef` are `repr(C)` and have the same fields in the same order
        unsafe { transmute(self) }
    }
}

impl<'app, 'editor> ContextMut<'app, 'editor> {
    pub fn register_action(&mut self, action: EditorAction) -> Result<()> {
        self.editor.register_action(self.app, action)
    }

    pub fn as_ref(&self) -> ContextRef<'_, '_> {
        ContextRef {
            editor: self.editor,
            app: self.app,
        }
    }

    pub fn as_mut(&mut self) -> ContextMut<'_, '_> {
        ContextMut {
            editor: self.editor,
            app: self.app,
        }
    }
}
