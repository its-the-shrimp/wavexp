use std::{
    ops::{Add, Sub, Range, Deref, AddAssign, SubAssign},
    fmt::{self, Display, Formatter},
    cmp::Ordering,
    borrow::Cow,
    iter::once,
    mem::{replace, variant_count, transmute},
    num::NonZeroUsize, rc::Rc};
use js_sys::Math::random;
use wasm_bindgen::JsCast;
use web_sys::{
    Path2d,
    AudioBuffer,
    AudioNode,
    AudioBufferOptions, PointerEvent, Event};
use yew::{
    html,
    Html,
    scheduler::Shared, AttrValue};
use wavexp_utils::{
    R64,
    R32,
    OptionExt,
    Pipe,
    default,
    r32,
    r64,
    ArrayExt,
    ArrayFrom,
    SliceExt,
    AppResult,
    AppResultUtils,
    BoolExt,
    js_function,
    AppError,
    SharedExt,
    AwareRefMut,
    ResultExt};
use crate::{
    input::{Slider, Button, Buttons, Cursor, GraphEditorCanvas, Counter},
    visual::{GraphEditor, GraphPoint},
    global::{AppContext, AppEvent, AppAction},
    sequencer::{Sequencer, PlaybackContext},
    sound_internals::{TimeStretcherNode, AudioInput, AudioInputKind}};

pub type MSecs = R64;
pub type Secs = R64;
pub type Beats = R64;

pub trait FromBeats {
    fn to_msecs(self, bps: Self) -> MSecs;
    fn to_secs(self, bps: Self) -> Secs;
    fn secs_to_beats(self, bps: Self) -> Beats;
}

impl FromBeats for Beats {
    fn to_secs(self, bps: Self) -> Secs {self / bps}
    fn to_msecs(self, bps: Self) -> MSecs {self * 1000u16 / bps}
    fn secs_to_beats(self, bps: Self) -> Beats {self * bps}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, PartialOrd, Ord)]
pub struct Note(u8);

impl Display for Note {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(unsafe{Self::NAMES.get_unchecked(self.0 as usize)}, f)
    }
}

impl Add<isize> for Note {
    type Output = Note;
    fn add(self, rhs: isize) -> Self::Output {
        self.checked_add(rhs).to_app_result().report().unwrap_or(self)
    }
}

impl AddAssign<isize> for Note {
    fn add_assign(&mut self, rhs: isize) {
        self.checked_add_assign(rhs).to_app_result().report();
    }
}

impl Sub<isize> for Note {
    type Output = Note;
    fn sub(self, rhs: isize) -> Self::Output {
        self.checked_sub(rhs).to_app_result().report().unwrap_or(self)
    }
}

impl SubAssign<isize> for Note {
    fn sub_assign(&mut self, rhs: isize) {
        self.checked_sub_assign(rhs).to_app_result().report();
    }
}

impl Note {
    pub const MAX: Note = Note(35);
    pub const N_NOTES: usize = Self::FREQS.len();
    pub const FREQS: [R32; 36] = [
        r32![65.410] /*C2*/, r32![69.300] /*C#2*/,
        r32![73.420] /*D2*/, r32![77.780] /*D#2*/,
        r32![82.410] /*E2*/,
        r32![87.310] /*F2*/, r32![92.500] /*F#2*/,
        r32![98.000] /*G2*/, r32![103.83] /*G#2*/,
        r32![110.00] /*A2*/, r32![116.54] /*A#2*/,
        r32![123.47] /*B2*/,
        r32![130.81] /*C3*/, r32![138.59] /*C#3*/,
        r32![146.83] /*D3*/, r32![155.56] /*D#3*/,
        r32![164.81] /*E3*/,
        r32![174.61] /*F3*/, r32![185.00] /*F#3*/,
        r32![196.00] /*G3*/, r32![207.65] /*G#3*/,
        r32![220.00] /*A3*/, r32![233.08] /*A#3*/,
        r32![246.94] /*B3*/,
        r32![261.63] /*C4*/, r32![277.18] /*C#4*/,
        r32![293.66] /*D4*/, r32![311.13] /*D#4*/,
        r32![329.63] /*E4*/,
        r32![349.23] /*F4*/, r32![369.99] /*F#4*/,
        r32![392.00] /*G4*/, r32![415.30] /*G#4*/,
        r32![440.00] /*A4*/, r32![466.16] /*A#4*/,
        r32![493.88] /*B4*/
    ];

    pub const NAMES: [&'static str; 36] = [
        "C2", "C#2",
        "D2", "D#2",
        "E2",
        "F2", "F#2",
        "G2", "G#2",
        "A2", "A#2",
        "B2",
        "C3", "C#3",
        "D3", "D#3",
        "E3",
        "F3", "F#3",
        "G3", "G#3",
        "A3", "A#3",
        "B3",
        "C4", "C#4",
        "D4", "D#4",
        "E4",
        "F4", "F#4",
        "G4", "G#4",
        "A4", "A#4",
        "B4"];

    pub fn checked_add(self, rhs: isize) -> Option<Self> {
        let new = self.0 as isize + rhs;
        if new >= 0 && new < Self::N_NOTES as isize {Some(Self(new as u8))}
        else {None}
    }

    pub fn checked_add_assign(&mut self, rhs: isize) -> bool {
        if let Some(x) = self.checked_add(rhs) {*self = x; true} else {false}
    }

    pub fn checked_sub(self, rhs: isize) -> Option<Self> {
        let new = self.0 as isize - rhs;
        if new >= 0 && new < Self::N_NOTES as isize {Some(Self(new as u8))}
        else {None}
    }

    pub fn checked_sub_assign(&mut self, rhs: isize) -> bool {
        if let Some(x) = self.checked_sub(rhs) {*self = x; true} else {false}
    }

    pub const fn from_index(value: usize) -> Self {
        if value >= Self::FREQS.len() {Self::MAX}
        else {Self(value as u8)}
    }

    pub const fn index(&self) -> usize {
        self.0 as usize
    }

    pub fn freq(&self) -> R32 {
        unsafe{*Self::FREQS.get_unchecked(self.0 as usize)}
    }

    pub const fn recip(self) -> Self {
        Self(Self::MAX.0 - self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SoundType {
    Note,
    Noise,
    Custom
}

impl SoundType {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Note => "Simple Wave",
            Self::Noise => "White Noise",
            Self::Custom => "Custom"
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NoteBlock {
    pub offset: Beats,
    pub value: Note,
    pub len: Beats
}

impl PartialOrd for NoteBlock {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.offset.cmp(&other.offset))
    }
}

impl Ord for NoteBlock {
    fn cmp(&self, other: &Self) -> Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl GraphPoint for NoteBlock {
    const EDITOR_NAME: &'static str = "Note Editor";
    const Y_BOUND: Range<R64> =
        r64![0] .. r64![Note::N_NOTES];
    const SCALE_Y_BOUND: Range<R64> = {
        let x = r64![10 - (Note::N_NOTES - 1) % 10 + Note::N_NOTES - 1];
        x .. x
    };
    const OFFSET_Y_BOUND: Range<R64> = {
        let x = r64![(10 - Note::N_NOTES as i8 % 10) / -2];
        x .. x
    };
    const Y_SNAP: R64 = r64![1];

    type Inner = Beats;
    type Y = Note;
    /// (sound block offset, number of repetitions of the pattern)
    type VisualContext = (Beats, NonZeroUsize);

    fn inner(&self) -> &Self::Inner {&self.len}
    fn inner_mut(&mut self) -> &mut Self::Inner {&mut self.len}

    fn y(&self) -> &Self::Y {&self.value}
    fn y_mut(&mut self) -> &mut Self::Y {&mut self.value}

    fn loc(&self) -> [R64; 2] {
        [self.offset, self.value.recip().index().into()]
    }

    fn move_point(point: Result<&mut Self, &mut [R64; 2]>, delta: [R64; 2], meta: bool) {
        match point {
            Ok(NoteBlock{offset, value, len}) => {
                if meta {
                    *len += delta[0];
                } else {
                    *offset += delta[0];
                    *offset = R64::ZERO.max(*offset);
                }
                *value -= delta[1].into();
            }
            Err(point) => {
                if !meta {
                    point[0] += delta[0];
                }
                point[1] += delta[1];
            }
        }
    }

    fn in_hitbox(
        &self,
        point: [R64; 2],
        _:     &AppContext,
        _:     &Sequencer,
        _:     Self::VisualContext
    ) -> AppResult<bool> {
        Ok(self.value.recip().index() == *point[1] as usize
            && (self.offset .. self.offset + self.len).contains(&point[0]))
    }

    fn fmt_loc(loc: [R64; 2]) -> String {
        format!("{:.3}, {}", loc[0], Note::from_index(loc[1].into()).recip())
    }

    fn on_move(
        editor: &mut GraphEditor<Self>,
        ctx:    &mut AppContext,
        _:      Cursor,
        _:      [R64; 2],
        point:  Option<usize>
    ) -> AppResult<()> {
        let Some(last) = editor.len().checked_sub(1) else {return Ok(())};
        Ok(if point.map_or_else(|| editor.selection().contains(&last), |x| x == last) {
            ctx.emit_event(AppEvent::RedrawEditorPlane)
        })
    }

    fn on_click(
        editor:        &mut GraphEditor<Self>,
        ctx:           &mut AppContext,
        cursor:        Cursor,
        pressed_at:    impl Deref<Target = [R64; 2]>,
        released_at:   impl Deref<Target = [R64; 2]>,
        old_selection: Option<&[usize]>
    ) -> AppResult<Option<AppAction>> {
        if !cursor.meta {return Ok(None)}

        let delta = released_at.sub(&pressed_at);
        Ok(if delta.all(|x| *x == 0) {
            if !editor.selection().is_empty() || old_selection.map_or(false, |x| !x.is_empty()) {
                return Ok(None)
            }
            let [offset, y] = *released_at;
            let value = Note::from_index(y.into()).recip();
            let block_id = editor.add_point(Self{offset, value, len: r64![1]});
            ctx.emit_event(AppEvent::RedrawEditorPlane);
            Some(AppAction::AddNoteBlock{block_id, offset, value})
        } else {
            ctx.emit_event(AppEvent::RedrawEditorPlane);
            let mut removed = Vec::with_capacity(editor.selection().len());
            let (delta_x, delta_y) = (delta[0], isize::from(delta[1]));
            editor.filter_selected(|x| x.1.len > 0, |mut x| {
                x.1.len -= delta_x;
                x.1.value += delta_y;
                removed.push(x)
            });
            match removed.len() {
                0 => None,
                n if n == removed.capacity() => Some(AppAction::RemoveNoteBlocks(removed.into_boxed_slice())),
                _ => Some(AppAction::StretchNoteBlocks{delta_x, delta_y, removed: removed.into_boxed_slice()})
            }
        })
    }

    fn on_plane_hover(
        editor: &mut GraphEditor<Self>,
        ctx:    &mut AppContext,
        cursor: Cursor,
        _:      impl Deref<Target = [R64; 2]>,
        first:  bool
    ) -> AppResult<()> {
        if !first && editor.last_cursor().is_some_and(|x| *x == *cursor) {return Ok(())}
        let [m, a] = match *cursor {
            Buttons{left: false, shift: true, ..} =>
                [Self::EDITOR_NAME.into(),
                "Press and hold to zoom".into()],
            Buttons{left: true, shift: true, ..} =>
                [Cow::from(Self::EDITOR_NAME) + ": zooming",
                "Release to stop".into()],
            Buttons{left: false, meta: false, ..} =>
                [Self::EDITOR_NAME.into(),
                "Hold & drag to move around (press Meta for actions)".into()],
            Buttons{left: false, meta: true, ..} =>
                [Self::EDITOR_NAME.into(),
                "Click to add note, hold & drag to select".into()],
            Buttons{left: true, meta: false, ..} => 
                [Cow::from(Self::EDITOR_NAME) + ": Moving",
                "Release to stop".into()],
            Buttons{left: true, meta: true, ..} =>
                [Cow::from(Self::EDITOR_NAME) + ": Selecting",
                "Release to select".into()]
        };
        Ok(ctx.emit_event(AppEvent::SetHint(m, a)))
    }

    fn on_point_hover(
        editor:   &mut GraphEditor<Self>,
        ctx:      &mut AppContext,
        cursor:   Cursor,
        point_id: usize,
        first:    bool
    ) -> AppResult<()> {
        if !first && editor.last_cursor().is_some_and(|x| x.left == cursor.left) {return Ok(())}
        let m = Self::fmt_loc(unsafe{editor.get_unchecked(point_id)}.loc());
        let [m, a] = match *cursor {
            Buttons{left: false, ..} =>
                [m.into(), "LMB to move, LMB + Meta to stretch".into()],
            Buttons{left: true, meta: false, ..} =>
                [(m + ": Moving").into(), "Release to stop".into()],
            Buttons{left: true, meta: true, ..} =>
                [(m + ": Stretching").into(), "Release to stop".into()],
        };
        Ok(ctx.emit_event(AppEvent::SetHint(m, a)))
    }

    fn on_selection_hover(
        editor: &mut GraphEditor<Self>,
        ctx:    &mut AppContext,
        cursor: Cursor,
        first:  bool
    ) -> AppResult<()> {
        if !first && editor.last_cursor().is_some_and(|x| x.left == cursor.left) {return Ok(())}
        let m = {let n = editor.selection().len(); format!("{n} note{}", if n == 1 {""} else {"s"})};
        let [m, a] = match *cursor {
            Buttons{left: false, ..} =>
                [m.into(), "LMB to move, LMB + Meta to stretch".into()],
            Buttons{left: true, meta: false, ..} =>
                [(m + ": Moving").into(), "Release to stop".into()],
            Buttons{left: true, meta: true, ..} =>
                [(m + ": Stretching").into(), "Release to stop".into()],
        };
        Ok(ctx.emit_event(AppEvent::SetHint(m, a)))
    }

    fn on_undo(
        editor: &mut GraphEditor<Self>,
        ctx:    &mut AppContext,
        action: &AppAction
    ) -> AppResult<()> {
        Ok(match action {
            AppAction::AddNoteBlock{block_id, ..} => {
                editor.remove_points(once(*block_id), drop)?;
                ctx.emit_event(AppEvent::RedrawEditorPlane)
            }

            AppAction::RemoveNoteBlocks(blocks) => {
                for &(at, b) in blocks.iter() {
                    unsafe{editor.insert_point(at, b)}
                }
                ctx.emit_event(AppEvent::RedrawEditorPlane)
            }

            &AppAction::StretchNoteBlocks{delta_x, delta_y, ref removed} => {
                for mut b in editor.iter_selection_mut() {
                    *b.inner() -= delta_x;
                    *b.y() += delta_y;
                }
                for &(id, b) in removed.iter() {
                    unsafe{editor.insert_point(id, b)}
                }
                editor.expand_selection(removed.iter().map(|(id, _)| *id))?;
            }

            _ => (),
        })
    }

    fn on_redo(
        editor: &mut GraphEditor<Self>,
        ctx:    &mut AppContext,
        action: &AppAction
    ) -> AppResult<()> {
        Ok(match action {
            &AppAction::AddNoteBlock{offset, value, block_id} => {
                unsafe{editor.insert_point(block_id, NoteBlock{offset, value, len: r64![1]})};
                ctx.emit_event(AppEvent::RedrawEditorPlane)
            }

            AppAction::RemoveNoteBlocks(blocks) => {
                editor.remove_points(blocks.iter().map(|(id, _)| *id), drop)?;
                ctx.emit_event(AppEvent::RedrawEditorPlane)
            }

            &AppAction::StretchNoteBlocks{delta_x, delta_y, ref removed} => {
                editor.remove_points(removed.iter().map(|(id, _)| *id), drop)?;
                for mut b in editor.iter_selection_mut() {
                    *b.inner() += delta_x;
                    *b.y() -= delta_y;
                }
            }
            _ => (),
        })
    }

    fn on_redraw(
        editor:             &mut GraphEditor<Self>,
        ctx:                &AppContext,
        sequencer:          &Sequencer, 
        canvas_size:        &[R64; 2],
        solid:              &Path2d,
        _:                  &Path2d,
        (sb_offset, n_reps): Self::VisualContext
    ) -> AppResult<()> {
        let step = &canvas_size.div(&editor.scale());
        let offset = &R64::array_from(editor.offset());
        for block in editor.iter() {
            let [x, y] = block.loc().mul(step).sub(offset);
            solid.rect(*x, *y, *block.len * *step[0], *step[1]);
        }

        let total_len = editor.last().map_or_default(|x| x.offset + x.len);
        Ok(if let PlaybackContext::All(start) = sequencer.playback_ctx() && start.is_finite() {
            let progress = (ctx.frame() - start).secs_to_beats(sequencer.bps()) - sb_offset;
            if progress < total_len * n_reps {
                editor.force_redraw();
                let x = R64::new_or(progress, *progress % *total_len) * step[0] - offset[0];
                solid.move_to(*x, 0.0);
                solid.line_to(*x, *canvas_size[1]);
            }
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CustomBlock {
    offset: R64,
    pitch: Note
}

impl PartialOrd for CustomBlock {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.offset.cmp(&other.offset))
    }
}

impl Ord for CustomBlock {
    fn cmp(&self, other: &Self) -> Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl GraphPoint for CustomBlock {
    const EDITOR_NAME: &'static str = NoteBlock::EDITOR_NAME;
    const Y_BOUND: Range<R64> = NoteBlock::Y_BOUND;
    const SCALE_Y_BOUND: Range<R64> = NoteBlock::SCALE_Y_BOUND;
    const OFFSET_Y_BOUND: Range<R64> = NoteBlock::OFFSET_Y_BOUND;
    const Y_SNAP: R64 = NoteBlock::Y_SNAP;

    type Inner = ();
    type Y = Note;
    /// (sound block offset, number of repetitions of the pattern, audio duration)
    type VisualContext = (Beats, NonZeroUsize, Beats);

    fn inner(&self) -> &Self::Inner {&()}
    fn inner_mut(&mut self) -> &mut Self::Inner {unsafe{transmute(self)}}

    fn y(&self) -> &Self::Y {&self.pitch}
    fn y_mut(&mut self) -> &mut Self::Y {&mut self.pitch}

    fn loc(&self) -> [R64; 2] {[self.offset, self.pitch.recip().index().into()]}

    fn move_point(point: Result<&mut Self, &mut [R64; 2]>, delta: [R64; 2], _: bool) {
        match point {
            Ok(p) => {
                p.offset += delta[0];
                p.pitch -= delta[1].into();
            }
            Err(p) => {
                p[0] += delta[0];
                p[1] -= delta[1];
            }
        }
    }

    fn in_hitbox(
        &self,
        point:     [R64; 2],
        _:         &AppContext,
        _:         &Sequencer,
        (.., len): Self::VisualContext
    ) -> AppResult<bool> {
        Ok(self.pitch.recip().index() == *point[1] as usize
            && (self.offset .. self.offset + len).contains(&point[0]))
    }

    fn fmt_loc(loc: [R64; 2]) -> String {
        format!("{:.3}, {}", loc[0], Note::from_index(loc[1].into()).recip())
    }

    fn on_move(
        editor: &mut GraphEditor<Self>,
        ctx:    &mut AppContext,
        _:      Cursor,
        _:      [R64; 2],
        point:  Option<usize>
    ) -> AppResult<()> {
        let Some(last) = editor.len().checked_sub(1) else {return Ok(())};
        Ok(if point.map_or_else(|| editor.selection().contains(&last), |x| x == last) {
            ctx.emit_event(AppEvent::RedrawEditorPlane)
        })
    }

    fn on_click(
        editor:        &mut GraphEditor<Self>,
        ctx:           &mut AppContext,
        cursor:        Cursor,
        pressed_at:    impl Deref<Target = [R64; 2]>,
        released_at:   impl Deref<Target = [R64; 2]>,
        old_selection: Option<&[usize]>
    ) -> AppResult<Option<AppAction>> {
        if !cursor.meta {return Ok(None)}

        let delta = released_at.sub(&pressed_at);
        Ok(if delta.all(|x| *x == 0) {
            if !editor.selection().is_empty() || old_selection.map_or(false, |x| !x.is_empty()) {
                return Ok(None)
            }
            let [offset, y] = *released_at;
            let block = Self{offset, pitch: Note::from_index(y.into()).recip()};
            let block_id = editor.add_point(block);
            ctx.emit_event(AppEvent::RedrawEditorPlane);
            Some(AppAction::AddCustomBlock(block_id, block))
        } else {
            let mut removed = Vec::with_capacity(editor.selection().len());
            let (delta_x, delta_y) = (delta[0], isize::from(delta[1]));
            editor.filter_selected(|_| true, |mut x| {
                x.1.offset -= delta_x;
                x.1.pitch += delta_y;
                removed.push(x)
            });
            editor.clear_selection_area();
            Some(AppAction::RemoveCustomBlocks(removed.into_boxed_slice()))
        })
    }

    fn on_plane_hover(
        editor: &mut GraphEditor<Self>,
        ctx:    &mut AppContext,
        cursor: Cursor,
        _:      impl Deref<Target = [R64; 2]>,
        first:  bool
    ) -> AppResult<()> {
        if !first && editor.last_cursor().is_some_and(|x| *x == *cursor) {return Ok(())}
        let m = Self::EDITOR_NAME.into();
        let [m, a] = match *cursor {
            Buttons{left: false, shift: true, ..} =>
                [m, "Press and hold to zoom".into()],
            Buttons{left: true, shift: true, ..} =>
                [m + ": Zooming", "Release to stop".into()],
            Buttons{left: false, meta: false, ..} =>
                [m, "Hold & drag to move around (press Meta for actions)".into()],
            Buttons{left: false, meta: true, ..} =>
                [m, "Click to add note, hold & drag to select".into()],
            Buttons{left: true, meta: false, ..} => 
                [m + ": Moving", "Release to stop".into()],
            Buttons{left: true, meta: true, ..} =>
                [m + ": Removing", "Release to remove blocks".into()]
        };
        Ok(ctx.emit_event(AppEvent::SetHint(m, a)))
    }

    fn on_point_hover(
        editor:   &mut GraphEditor<Self>,
        ctx:      &mut AppContext,
        cursor:   Cursor,
        point_id: usize,
        first:    bool
    ) -> AppResult<()> {
        if !first && editor.last_cursor().is_some_and(|x| x.left == cursor.left) {return Ok(())}
        let m = Self::fmt_loc(unsafe{editor.get_unchecked(point_id)}.loc()).into();
        let [m, a] = match *cursor {
            Buttons{left: false, ..} =>
                [m, "LMB to move, LMB + Meta to remove".into()],
            Buttons{left: true, meta: false, ..} =>
                [m + ": Moving", "Release to stop".into()],
            Buttons{left: true, meta: true, ..} =>
                [m + ": Removing", "Release to remove".into()],
        };
        Ok(ctx.emit_event(AppEvent::SetHint(m, a)))
    }

    fn on_selection_hover(
        editor: &mut GraphEditor<Self>,
        ctx:    &mut AppContext,
        cursor: Cursor,
        first:  bool
    ) -> AppResult<()> {
        if !first && editor.last_cursor().is_some_and(|x| x.left == cursor.left) {return Ok(())}
        let n = editor.selection().len();
        let m = format!("{n} note{}", if n == 1 {""} else {"s"}).into();
        let [m, a] = match *cursor {
            Buttons{left: false, ..} =>
                [m, "LMB to move, LMB + Meta to remove".into()],
            Buttons{left: true, meta: false, ..} =>
                [m + ": Moving", "Release to stop".into()],
            Buttons{left: true, meta: true, ..} =>
                [m + ": Removing", "Release to remove".into()],
        };
        Ok(ctx.emit_event(AppEvent::SetHint(m, a)))
    }

    fn on_undo(
        editor: &mut GraphEditor<Self>,
        ctx:    &mut AppContext,
        action: &AppAction
    ) -> AppResult<()> {
        Ok(match action {
            &AppAction::AddCustomBlock(at, _) => {
                editor.remove_points(once(at), drop)?;
                ctx.emit_event(AppEvent::RedrawEditorPlane)
            }

            AppAction::RemoveCustomBlocks(blocks) => {
                for &(at, b) in blocks.as_ref() {
                    unsafe{editor.insert_point(at, b)}
                }
                ctx.emit_event(AppEvent::RedrawEditorPlane)
            }

            _ => (),
        })
    }

    fn on_redo(
        editor: &mut GraphEditor<Self>,
        ctx:    &mut AppContext,
        action: &AppAction
    ) -> AppResult<()> {
        Ok(match action {
            &AppAction::AddCustomBlock(at, block) => {
                unsafe{editor.insert_point(at, block)};
                ctx.emit_event(AppEvent::RedrawEditorPlane)
            }

            AppAction::RemoveCustomBlocks(removed) => {
                editor.remove_points(removed.iter().map(|(id, _)| *id), drop)?;
                ctx.emit_event(AppEvent::RedrawEditorPlane)
            }

            _ => (),
        })
    }

    fn on_redraw(
        editor:                   &mut GraphEditor<Self>,
        ctx:                      &AppContext,
        sequencer:                &Sequencer,
        canvas_size:              &[R64; 2],
        solid:                    &Path2d,
        _:                        &Path2d,
        (sb_offset, n_reps, len): Self::VisualContext
    ) -> AppResult<()> {
        let len = len.secs_to_beats(sequencer.bps());
        let step = &canvas_size.div(&editor.scale());
        let offset = &R64::array_from(editor.offset());
        for block in editor.iter() {
            let [x, y] = block.loc().mul(step).sub(offset);
            solid.rect(*x, *y, *len * *step[0], *step[1]);
        }

        let total_len = editor.last().map_or_default(|x| x.offset + len);
        Ok(if let PlaybackContext::All(start) = sequencer.playback_ctx() && start.is_finite() {
            let progress = (ctx.frame() - start).secs_to_beats(sequencer.bps()) - sb_offset;
            if progress < total_len * n_reps {
                editor.force_redraw();
                let x = R64::new_or(progress, *progress % *total_len) * step[0] - offset[0];
                solid.move_to(*x, 0.0);
                solid.line_to(*x, *canvas_size[1]);
            }
        })
    }
}

#[derive(Default, Debug, Clone)]
pub enum Sound {
    #[default] None,
    Note{pattern: Shared<GraphEditor<NoteBlock>>,
        volume: R32, attack: Beats, decay: Beats, sustain: R32, release: Beats,
        rep_count: NonZeroUsize},
    Noise{pattern: Shared<GraphEditor<NoteBlock>>, src: AudioBuffer,
        volume: R32, attack: Beats, decay: Beats, sustain: R32, release: Beats,
        rep_count: NonZeroUsize},
    Custom{pattern: Shared<GraphEditor<CustomBlock>>, src: Shared<AudioInput>,
        volume: R32, attack: Beats, decay: Beats, sustain: R32, release: Beats,
        rep_count: NonZeroUsize, speed: R32}
}

impl Sound {
    pub const TYPES: [SoundType; variant_count::<Self>() - 1 /* None */] = [
        SoundType::Note,
        SoundType::Noise,
        SoundType::Custom
    ];

    pub fn new(sound_type: SoundType) -> AppResult<Self> {
        Ok(match sound_type {
            SoundType::Note =>
                Self::Note{pattern: default(),
                    volume: r32![1], attack: r64![0], decay: r64![0], sustain: r32![1], release: r64![0],
                    rep_count: NonZeroUsize::MIN},

            SoundType::Noise => {
                let mut buf = vec![0.0; Sequencer::SAMPLE_RATE as usize]; // 1 second of noise
                buf.fill_with(|| random() as f32 * 2.0 - 1.0);
                let src = AudioBufferOptions::new(Sequencer::SAMPLE_RATE, Sequencer::SAMPLE_RATE as f32)
                    .number_of_channels(Sequencer::CHANNEL_COUNT)
                    .pipe(|x| AudioBuffer::new(x))?;
                for i in 0 .. Sequencer::CHANNEL_COUNT as i32 {
                    src.copy_to_channel(&buf, i)?;
                }
                Self::Noise{pattern: default(), src,
                    volume: r32![0.2], attack: r64![0], decay: r64![0], sustain: r32![1], release: r64![0.2],
                    rep_count: NonZeroUsize::MIN}
            }

            SoundType::Custom => 
                Self::Custom{pattern: default(),
                    src: default(),
                    volume: r32![1], attack: r64![0], decay: r64![0], sustain: r32![1], release: r64![0],
                    rep_count: NonZeroUsize::MIN, speed: r32![1]}
        })
    }

    pub fn name(&self) -> &'static str {
        match self {
            Self::None => "Undefined",
            Self::Note{..} => "Simple Wave",
            Self::Noise{..} => "White Noise",
            Self::Custom{..} => "Custom"
        }
    }

    pub fn play(&mut self, plug: &AudioNode, now: Secs, self_offset: Secs, bps: Beats) -> AppResult<()> {
        let ctx = plug.context();
        Ok(match *self {
            Sound::None => (),

            Sound::Note{ref pattern, volume, attack, decay, mut sustain, release, rep_count} => {
                let pat = pattern.get()?;
                let Some(last) = pat.last() else {return Ok(())};
                let pat_len = (last.offset + last.len).to_secs(bps);

                for rep in 0 .. rep_count.get() {
                    for NoteBlock{offset, value, len} in pat.iter() {
                        let block = ctx.create_gain()?;
                        let gain = block.gain();
                        let start = now + self_offset + pat_len * rep + offset.to_secs(bps);
                        let mut at = start;
                        gain.set_value(0.0);
                        at += attack.to_secs(bps);
                        gain.linear_ramp_to_value_at_time(*volume, *at)?;
                        at += decay.to_secs(bps);
                        sustain *= volume;
                        gain.linear_ramp_to_value_at_time(*sustain, *at)?;
                        at = start + len.to_secs(bps);
                        gain.set_value_at_time(*sustain, *at)?;
                        at += release.to_secs(bps);
                        gain.linear_ramp_to_value_at_time(0.0, *at)?;

                        let block_core = ctx.create_oscillator()?;
                        block_core.frequency().set_value(*value.freq());
                        block_core.connect_with_audio_node(&block)?
                            .connect_with_audio_node(plug)?;
                        block_core.start_with_when(*start)?;
                        block_core.stop_with_when(*at)?;
                        block_core.clone().set_onended(Some(&js_function!(|| {
                            block.disconnect().map_err(AppError::from).report();
                            block_core.disconnect().map_err(AppError::from).report();
                        })));
                    }
                }
            }

            Sound::Noise{ref pattern, ref src, volume, attack, decay, mut sustain, release, rep_count} => {
                let pat = pattern.get()?;
                let Some(last) = pat.last() else {return Ok(())};
                let pat_len = (last.offset + last.len).to_secs(bps);

                for rep in 0 .. rep_count.get() {
                    // TODO: handle `value`
                    for NoteBlock{offset, len, ..} in pat.iter() {
                        let block = ctx.create_gain()?;
                        let gain = block.gain();
                        let start = now + self_offset + pat_len * rep + offset.to_secs(bps);
                        let mut at = start;
                        gain.set_value(0.0);
                        at += attack.to_secs(bps);
                        gain.linear_ramp_to_value_at_time(*volume, *at)?;
                        at += decay.to_secs(bps);
                        sustain *= volume;
                        gain.linear_ramp_to_value_at_time(*sustain, *at)?;
                        at = start + len.to_secs(bps);
                        gain.set_value_at_time(*sustain, *at)?;
                        at += release.to_secs(bps);
                        gain.linear_ramp_to_value_at_time(0.0, *at)?;

                        let block_core = ctx.create_buffer_source()?;
                        block_core.set_buffer(Some(src));
                        if at - start > 1 {
                            block_core.set_loop(true);
                        }
                        block_core.connect_with_audio_node(&block)?
                            .connect_with_audio_node(plug)?;
                        block_core.start_with_when(*start)?;
                        block_core.stop_with_when(*at)?;
                        block_core.clone().set_onended(Some(&js_function!(|| {
                            block.disconnect().map_err(AppError::from).report();
                            block_core.disconnect().map_err(AppError::from).report();
                        })));
                    }
                }
            }

            Sound::Custom{ref pattern, ref src, volume, attack, decay, mut sustain, release, rep_count, speed} => {
                let pat = pattern.get()?;
                let Some(last) = pat.last() else {return Ok(())};
                let src = src.get()?;
                let len = src.duration() / speed;
                let pat_len = last.offset.to_secs(bps) + len;

                for rep in 0 .. rep_count.get() {
                    // TODO: handle `pitch`
                    for CustomBlock{offset, ..} in pat.iter() {
                        let block = ctx.create_gain()?;
                        let gain = block.gain();
                        let start = now + self_offset + pat_len * rep + offset.to_secs(bps);
                        let mut at = start;
                        gain.set_value(0.0);
                        at += attack.to_secs(bps);
                        gain.linear_ramp_to_value_at_time(*volume, *at)?;
                        at += decay.to_secs(bps);
                        sustain *= volume;
                        gain.linear_ramp_to_value_at_time(*sustain, *at)?;
                        at = start + len;
                        gain.set_value_at_time(*sustain, *at)?;
                        at += release.to_secs(bps);
                        gain.linear_ramp_to_value_at_time(0.0, *at)?;

                        let stretcher = TimeStretcherNode::new(&ctx)?;
                        stretcher.rate().set_value(1.0 / *speed);

                        let block_core = ctx.create_buffer_source()?;
                        block_core.set_buffer(src.inner());
                        //block_core.playback_rate().set_value(*speed);
                        block_core
                            .connect_with_audio_node(&stretcher)?
                            .connect_with_audio_node(&block)?
                            .connect_with_audio_node(plug)?;
                        block_core.start_with_when(*start)?;
                        block_core.clone().set_onended(Some(&js_function!(|| {
                            let res: AppResult<_> = try {
                                block.disconnect()?;
                                stretcher.disconnect()?;
                                block_core.disconnect()?;
                            };
                            res.report();
                        })));
                    }
                }
            }
        })
    }

    pub fn len(&self, bps: Beats) -> AppResult<Beats> {
        Ok(match self {
            Self::None => r64![1],

            Self::Note {pattern, ..} |
            Self::Noise{pattern, ..} => pattern.get()?
                .last().map_or_default(|x| x.offset + x.len),

            Self::Custom{pattern, src, speed, ..} => match pattern.get()?.last() {
                Some(x) => x.offset + src.get()?.duration().secs_to_beats(bps) / speed,
                None => default()
            }
        })
    }

    pub fn rep_count(&self) -> NonZeroUsize {
        match *self {
            Self::None => NonZeroUsize::MIN,
            Self::Note  {rep_count, ..} |
            Self::Noise {rep_count, ..} |
            Self::Custom{rep_count, ..} => rep_count
        }
    }

    pub fn params(&self, ctx: &AppContext, sequencer: &Sequencer) -> Html {
        let setter = ctx.event_emitter();
        match self {
            Self::None => html!{<div class="horizontal-menu">
                {for Sound::TYPES.iter().map(|x| html!{
                    <Button name={x.name()}
                        setter={setter.reform(|_| AppEvent::SetBlockType(*x))}>
                        <p>{x.name()}</p>
                    </Button>
                })}
            </div>},

            Self::Note {pattern, volume, attack, decay, sustain, release, rep_count, ..} |
            Self::Noise{pattern, volume, attack, decay, sustain, release, rep_count, ..} => match ctx.selected_tab() {
                0 /* General */ => html!{<div id="inputs">
                    <Slider key="noise-vol"
                    setter={setter.reform(|x| AppEvent::Volume(R32::from(x)))}
                    name="Noise Volume"
                    initial={*volume}/>
                    <Counter key="noise-repcnt"
                    setter={setter.reform(|x| AppEvent::RepCount(NonZeroUsize::from(x)))}
                    fmt={|x| format!("{}", usize::from(x))}
                    name="Number Of Pattern Repetitions"
                    min={r64![1]}
                    initial={*rep_count}/>
                </div>},
                1 /* Envelope */ => html!{<div id="inputs">
                    <Counter key="noise-att"
                    setter={setter.reform(AppEvent::Attack)}
                    name="Noise Attack Time" postfix="Beats"
                    initial={*attack}/>
                    <Counter key="noise-dec"
                    setter={setter.reform(AppEvent::Decay)}
                    name="Noise Decay Time" postfix="Beats"
                    initial={*decay}/>
                    <Slider key="noise-sus"
                    setter={setter.reform(|x| AppEvent::Sustain(R32::from(x)))}
                    name="Noise Sustain Level"
                    initial={*sustain}/>
                    <Counter key="noise-rel"
                    setter={setter.reform(AppEvent::Release)}
                    name="Noise Release Time" postfix="Beats"
                    initial={*release}/>
                </div>},
                2 /* Pattern */ => html!{
                    <GraphEditorCanvas<NoteBlock> editor={pattern} emitter={setter}/>
                },
                tab_id => html!{<p style="color:red">{format!("Invalid tab ID: {tab_id}")}</p>}
            }

            Self::Custom{pattern, volume, attack, decay, sustain, release, rep_count, speed, src} =>
            match ctx.selected_tab() {
                0 /* General */ => html!{<div id="inputs">
                    <Slider key="custom-vol"
                    setter={setter.reform(|x| AppEvent::Volume(R32::from(x)))}
                    name="Noise Volume"
                    initial={*volume}/>
                    <Counter key="custom-repcnt"
                    setter={setter.reform(|x| AppEvent::RepCount(NonZeroUsize::from(x)))}
                    fmt={|x| format!("{:.0}", x)}
                    name="Number Of Pattern Repetitions"
                    min={r64![1]}
                    initial={*rep_count}/>
                    <Counter key="note-speed"
                    setter={setter.reform(|x| AppEvent::Speed(R32::from(x)))}
                    fmt={|x| format!("{x:.2}x")}
                    name="Playback speed"
                    initial={*speed}/>
                    <Button name="Audio input" help="Click to change" class="wide"
                    setter={setter.reform(|_| AppEvent::TogglePopup)}>
                        if let Some(input) = src.get().report() {
                            <div class="inner-button-panel">
                                if input.playing() {
                                    <Button name="Stop playing" help="Click to stop the playback"
                                    setter={{
                                        let s = Rc::clone(src);
                                        setter.reform(move |e: PointerEvent| {
                                            e.stop_propagation();
                                            AppEvent::StopPlay(Some(s.clone()))
                                        })
                                    }}>
                                        <svg viewBox="0 0 100 100">
                                            <polygon points="25,25 75,25 75,75 25,75"/>
                                        </svg>
                                    </Button>
                                } else if let AudioInputKind::Empty = input.kind() {
                                    <Button class="unavailable" name="Play audio input (not chosen)"
                                    help="Choose the audio input for the sound block to play it here"
                                    setter={|e: PointerEvent| e.stop_propagation()}>
                                        <svg viewBox="0 0 100 100">
                                            <polygon points="25,25 75,50 25,75"/>
                                        </svg>
                                    </Button>
                                } else {
                                    <Button name="Play audio input" help="Click to hear how the input sounds"
                                    setter={{
                                        let s = Rc::clone(src);
                                        setter.reform(move |e: PointerEvent| {
                                            e.stop_propagation();
                                            AppEvent::PreparePlay(Some(s.clone()))
                                        })
                                    }}>
                                        <svg viewBox="0 0 100 100">
                                            <polygon points="25,25 75,50 25,75"/>
                                        </svg>
                                    </Button>
                                }
                                <p>{input.add_ctx(sequencer).to_string()}</p>
                            </div>
                        } else {
                            <p style="color:red">{"Failed to access audio input"}</p>
                        }
                    </Button>
                </div>},
                1 /* Envelope */ => html!{<div id="inputs">
                    <Counter key="custom-att"
                    setter={setter.reform(AppEvent::Attack)}
                    name="Noise Attack Time" postfix="Beats"
                    initial={*attack}/>
                    <Counter key="custom-dec"
                    setter={setter.reform(AppEvent::Decay)}
                    name="Noise Decay Time" postfix="Beats"
                    initial={*decay}/>
                    <Slider key="custom-sus"
                    setter={setter.reform(|x| AppEvent::Sustain(R32::from(x)))}
                    name="Noise Sustain Level"
                    initial={*sustain}/>
                    <Counter key="custom-rel"
                    setter={setter.reform(AppEvent::Release)}
                    name="Noise Release Time" postfix="Beats"
                    initial={*release}/>
                </div>},
                2 /* Pattern */ => html!{
                    <GraphEditorCanvas<CustomBlock> editor={pattern} emitter={setter}/>
                },
                tab_id => html!{<p style="color:red">{format!("Invalid tab ID: {tab_id}")}</p>}
            }
        }
    }

    /// The returned string is the popup title
    pub fn popup(&self, ctx: &AppContext, sequencer: &Sequencer) -> (AttrValue, Html) {
        let body = html!{
            <div class="dark-bg horizontal-menu-wrapper">
                {sequencer.inputs(ctx.event_emitter().clone())}
            </div>
        };
        ("Choose audio input".into(), body)
    }

    pub fn handle_event(
        &mut self,
        event: &AppEvent,
        ctx: &mut AppContext,
        sequencer: &AwareRefMut<'_, Sequencer>,
        offset: Beats
    ) -> AppResult<()> {
        Ok(match self {
            Sound::None => match event {
                &AppEvent::SetBlockType(ty) => {
                    *self = Self::new(ty)?;
                    ctx.register_action(AppAction::SetBlockType(ty));
                    ctx.emit_event(AppEvent::RedrawEditorPlane);
                }

                AppEvent::Redo(actions) => for action in actions.iter() {
                    if let &AppAction::SetBlockType(ty) = action {
                        *self = Self::new(ty)?;
                        ctx.emit_event(AppEvent::RedrawEditorPlane);
                    }
                }

                _ => (),
            }

            Sound::Note {pattern, volume, attack, decay, sustain, release, rep_count} |
            Sound::Noise{pattern, volume, attack, decay, sustain, release, rep_count, ..} => match event {
                &AppEvent::Volume(to) =>
                    ctx.register_action(AppAction::SetVolume{from: replace(volume, to), to}),
                &AppEvent::Attack(to) =>
                    ctx.register_action(AppAction::SetAttack{from: replace(attack, to), to}),
                &AppEvent::Decay(to) =>
                    ctx.register_action(AppAction::SetDecay{from: replace(decay, to), to}),
                &AppEvent::Sustain(to) =>
                    ctx.register_action(AppAction::SetSustain{from: replace(sustain, to), to}),
                &AppEvent::Release(to) =>
                    ctx.register_action(AppAction::SetRelease{from: replace(release, to), to}),
                &AppEvent::RepCount(to) => {
                    ctx.register_action(AppAction::SetRepCount{from: replace(rep_count, to), to});
                    ctx.emit_event(AppEvent::RedrawEditorPlane);
                }

                e => {
                    let pattern = if ctx.selected_tab() == 2 {
                        let mut p = pattern.get_mut()?;
                        p.handle_event(e, ctx, sequencer, || (offset, *rep_count))?;
                        Some(p)
                    } else {None};

                    match e {
                        AppEvent::Undo(actions) => for action in actions.iter() {
                            match *action {
                                AppAction::SetBlockType(_) => {
                                    drop(pattern);
                                    *self = default();
                                    break
                                }

                                AppAction::SetVolume  {from, ..} => *volume    = from,
                                AppAction::SetAttack  {from, ..} => *attack    = from,
                                AppAction::SetDecay   {from, ..} => *decay     = from,
                                AppAction::SetSustain {from, ..} => *sustain   = from,
                                AppAction::SetRelease {from, ..} => *release   = from,
                                AppAction::SetRepCount{from, ..} => {
                                    *rep_count = from;
                                    ctx.emit_event(AppEvent::RedrawEditorPlane)
                                }
                                _ => (),
                            }
                        }

                        AppEvent::Redo(actions) => for action in actions.iter() {
                            match *action {
                                AppAction::SetVolume  {to, ..} => *volume    = to,
                                AppAction::SetAttack  {to, ..} => *attack    = to,
                                AppAction::SetDecay   {to, ..} => *decay     = to,
                                AppAction::SetSustain {to, ..} => *sustain   = to,
                                AppAction::SetRelease {to, ..} => *release   = to,
                                AppAction::SetRepCount{to, ..} => {
                                    *rep_count = to;
                                    ctx.emit_event(AppEvent::RedrawEditorPlane)
                                }
                                _ => (),
                            }
                        }

                        _ => (),
                    }
                }
            }

            Sound::Custom{pattern, src, volume, attack, decay, sustain, release, rep_count, speed, ..} =>
            match event {
                &AppEvent::Volume(to) =>
                    ctx.register_action(AppAction::SetVolume{from: replace(volume, to), to}),
                &AppEvent::Attack(to) =>
                    ctx.register_action(AppAction::SetAttack{from: replace(attack, to), to}),
                &AppEvent::Decay(to) =>
                    ctx.register_action(AppAction::SetDecay{from: replace(decay, to), to}),
                &AppEvent::Sustain(to) =>
                    ctx.register_action(AppAction::SetSustain{from: replace(sustain, to), to}),
                &AppEvent::Release(to) =>
                    ctx.register_action(AppAction::SetRelease{from: replace(release, to), to}),
                &AppEvent::RepCount(to) => {
                    ctx.register_action(AppAction::SetRepCount{from: replace(rep_count, to), to});
                    ctx.emit_event(AppEvent::RedrawEditorPlane);
                }
                &AppEvent::Speed(to) => {
                    ctx.register_action(AppAction::SetSpeed{from: replace(speed, to), to});
                    ctx.emit_event(AppEvent::RedrawEditorPlane);
                }

                AppEvent::AddInput(to) | AppEvent::SelectInput(to) => {
                    ctx.register_action(AppAction::SelectInput{from: src.clone(), to: to.clone()});
                    *src = to.clone();
                    ctx.emit_event(AppEvent::RedrawEditorPlane)
                }

                e => {
                    if ctx.selected_tab() == 2 {
                        pattern.get_mut()?
                            .handle_event(e, ctx, sequencer, ||
                                (offset, *rep_count, src.get().map_or_default(|x|
                                    x.add_ctx(sequencer).duration() / *speed)))?;
                    }

                    match e {
                        AppEvent::Undo(actions) => for action in actions.iter() {
                            match *action {
                                AppAction::SetBlockType(_) => {
                                    *self = default();
                                    break
                                }

                                AppAction::SetVolume  {from, ..} => *volume    = from,
                                AppAction::SetAttack  {from, ..} => *attack    = from,
                                AppAction::SetDecay   {from, ..} => *decay     = from,
                                AppAction::SetSustain {from, ..} => *sustain   = from,
                                AppAction::SetRelease {from, ..} => *release   = from,
                                AppAction::SetRepCount{from, ..} => {
                                    *rep_count = from;
                                    ctx.emit_event(AppEvent::RedrawEditorPlane)
                                }
                                AppAction::SetSpeed{from, ..} => {
                                    *speed = from;
                                    ctx.emit_event(AppEvent::RedrawEditorPlane)
                                }
                                AppAction::SelectInput{ref from, ..} => {
                                    *src = from.clone();
                                    ctx.emit_event(AppEvent::RedrawEditorPlane)
                                }
                                _ => (),
                            }
                        }

                        AppEvent::Redo(actions) => for action in actions.iter() {
                            match *action {
                                AppAction::SetVolume  {to, ..} => *volume    = to,
                                AppAction::SetAttack  {to, ..} => *attack    = to,
                                AppAction::SetDecay   {to, ..} => *decay     = to,
                                AppAction::SetSustain {to, ..} => *sustain   = to,
                                AppAction::SetRelease {to, ..} => *release   = to,
                                AppAction::SetRepCount{to, ..} => {
                                    *rep_count = to;
                                    ctx.emit_event(AppEvent::RedrawEditorPlane)
                                }
                                AppAction::SetSpeed{to, ..} => {
                                    *speed = to;
                                    ctx.emit_event(AppEvent::RedrawEditorPlane)
                                }
                                AppAction::SelectInput{ref to, ..} => {
                                    *src = to.clone();
                                    ctx.emit_event(AppEvent::RedrawEditorPlane)
                                }
                                _ => (),
                            }
                        }

                        _ => (),
                    }
                }
            }
        })
    }
}
