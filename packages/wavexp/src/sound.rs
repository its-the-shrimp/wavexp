use std::{
    ops::{Add, Sub, Range, Deref, Not, DerefMut, AddAssign, SubAssign},
    fmt::{self, Display, Formatter},
    cmp::Ordering,
    rc::Rc,
    borrow::Cow,
    cell::{RefCell, LazyCell},
    iter::once,
    mem::{replace, variant_count, transmute},
    num::NonZeroUsize};
use js_sys::Math::random;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::{JsFuture, spawn_local};
use web_sys::{
    AudioNode,
    AudioContext,
    AudioBuffer,
    GainNode,
    Path2d,
    DynamicsCompressorNode,
    AnalyserNode,
    HtmlCanvasElement,
    HtmlInputElement};
use yew::{
    html,
    Html,
    Callback,
    scheduler::Shared,
    TargetCast};
use wavexp_utils::{
    JsResult,
    JsResultUtils,
    R64,
    R32,
    OptionExt,
    Pipe,
    document,
    VecExt,
    Take,
    default,
    report_err,
    r32,
    r64,
    js_try,
    ArrayExt,
    ArrayFrom,
    SliceExt,
    BoolExt,
    ResultExt, js_log};
use crate::{
    input::{Slider, Button, Buttons, Cursor, GraphEditorCanvas, Counter},
    visual::{GraphEditor, GraphPoint},
    global::{AppContext, AppEvent, AppAction}};

pub type MSecs = R64;
pub type Secs = R64;
pub type Beats = R64;

pub trait FromBeats {
    fn to_msecs(self, bps: Self) -> MSecs;
    fn to_secs(self, bps: Self) -> Secs;
    fn secs_to_beats(self, bps: Self) -> Beats;
}

impl FromBeats for Beats {
    #[inline]
    fn to_secs(self, bps: Self) -> Secs {self / bps}

    #[inline]
    fn to_msecs(self, bps: Self) -> MSecs {self * 1000u16 / bps}

    #[inline]
    fn secs_to_beats(self, bps: Self) -> Beats {self * bps}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, PartialOrd, Ord)]
pub struct Note(u8);

impl Display for Note {
    #[inline] fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(unsafe{Self::NAMES.get_unchecked(self.0 as usize)}, f)
    }
}

impl Add<isize> for Note {
    type Output = Note;
    #[inline] fn add(self, rhs: isize) -> Self::Output {
        self.checked_add(rhs).report_none().unwrap_or(self)
    }
}

impl AddAssign<isize> for Note {
    #[inline] fn add_assign(&mut self, rhs: isize) {
        self.checked_add_assign(rhs).report_false();
    }
}

impl Sub<isize> for Note {
    type Output = Note;
    #[inline] fn sub(self, rhs: isize) -> Self::Output {
        self.checked_sub(rhs).report_none().unwrap_or(self)
    }
}

impl SubAssign<isize> for Note {
    #[inline] fn sub_assign(&mut self, rhs: isize) {
        self.checked_sub_assign(rhs).report_false();
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

    #[inline] pub fn checked_add(self, rhs: isize) -> Option<Self> {
        let new = self.0 as isize + rhs;
        if new >= 0 && new < Self::N_NOTES as isize {Some(Self(new as u8))}
        else {None}
    }

    #[inline] pub fn checked_add_assign(&mut self, rhs: isize) -> bool {
        if let Some(x) = self.checked_add(rhs) {*self = x; true} else {false}
    }

    #[inline] pub fn checked_sub(self, rhs: isize) -> Option<Self> {
        let new = self.0 as isize - rhs;
        if new >= 0 && new < Self::N_NOTES as isize {Some(Self(new as u8))}
        else {None}
    }

    #[inline] pub fn checked_sub_assign(&mut self, rhs: isize) -> bool {
        if let Some(x) = self.checked_sub(rhs) {*self = x; true} else {false}
    }

    #[inline] pub const fn from_index(value: usize) -> Self {
        if value >= Self::FREQS.len() {Self::MAX}
        else {Self(value as u8)}
    }

    #[inline] pub const fn index(&self) -> usize {
        self.0 as usize
    }

    #[inline] pub fn freq(&self) -> R32 {
        unsafe{*Self::FREQS.get_unchecked(self.0 as usize)}
    }

    #[inline] pub const fn recip(self) -> Self {
        Self(Self::MAX.0 - self.0)
    }
}

pub struct TabInfo {
    pub name: &'static str
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SoundType {
    Note,
    Noise,
    Custom
}

impl SoundType {
    #[inline] pub fn name(&self) -> &'static str {
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
    #[inline] fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.offset.cmp(&other.offset))
    }
}

impl Ord for NoteBlock {
    #[inline] fn cmp(&self, other: &Self) -> Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl GraphPoint for NoteBlock {
    const EDITOR_NAME: &'static str = "Note Editor";
    // TODO: make this generic over the number of defined notes
    const Y_BOUND: Range<R64> = r64![0.0] .. r64![36.0];
    const SCALE_Y_BOUND: Range<R64> = r64![40.0] .. r64![40.0];
    const OFFSET_Y_BOUND: Range<R64> = r64![-2.0] .. r64![-2.0];
    const Y_SNAP: R64 = r64![1.0];

    type Inner = Beats;
    type Y = Note;
    /// number of repetitions of the pattern
    type VisualContext = NonZeroUsize;

    #[inline] fn inner(&self) -> &Self::Inner {&self.len}
    #[inline] fn inner_mut(&mut self) -> &mut Self::Inner {&mut self.len}

    #[inline] fn y(&self) -> &Self::Y {&self.value}
    #[inline] fn y_mut(&mut self) -> &mut Self::Y {&mut self.value}

    #[inline] fn loc(&self) -> [R64; 2] {
        [self.offset, self.value.recip().index().into()]
    }

    #[inline] fn move_point(point: Result<&mut Self, &mut [R64; 2]>, delta: [R64; 2], meta: bool) {
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
                point[1] -= delta[1];
            }
        }
    }

    #[inline] fn in_hitbox(&self, point: [R64; 2], _: Self::VisualContext) -> bool {
        self.value.recip().index() == *point[1] as usize
            && (self.offset .. self.offset + self.len).contains(&point[0])
    }

    #[inline] fn fmt_loc(loc: [R64; 2]) -> String {
        format!("{:.3}, {}", loc[0], Note::from_index(loc[1].into()).recip())
    }

    #[inline] fn on_move(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        _: Cursor,
        _: [R64; 2],
        point: Option<usize>
    ) {
        let last = editor.len() - 1;
        if point.map_or_else(|| editor.selection().contains(&last), |x| x == last) {
            ctx.emit_event(AppEvent::RedrawEditorPlane)
        }
    }

    fn on_click(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        cursor: Cursor,
        pressed_at:    impl Deref<Target = [R64; 2]>,
        released_at:   impl Deref<Target = [R64; 2]>,
        old_selection: Option<&[usize]>
    ) -> Option<AppAction> {
        if !cursor.meta {return None}

        let delta = released_at.sub(&pressed_at);
        if delta.all(|x| *x == 0) {
            if !editor.selection().is_empty() || old_selection.map_or(false, |x| !x.is_empty()) {return None}

            let [offset, y] = *released_at;
            let value = Note::from_index(y.into()).recip();
            let block_id = editor.add_point(Self{offset, value, len: r64![1.0]});
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
        }
    }

    #[inline] fn on_plane_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        cursor: Cursor,
        _: impl Deref<Target = [R64; 2]>,
        first: bool
    ) {
        if !first && editor.last_cursor().is_some_and(|x| *x == *cursor) {return}
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
        ctx.emit_event(AppEvent::SetHint(m, a))
    }

    #[inline] fn on_point_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        cursor: Cursor,
        point_id: usize,
        first: bool
    ) {
        if !first && editor.last_cursor().is_some_and(|x| x.left == cursor.left) {return}
        let m = Self::fmt_loc(unsafe{editor.get_unchecked(point_id)}.loc());
        let [m, a] = match *cursor {
            Buttons{left: false, ..} =>
                [m.into(), "LMB to move, LMB + Meta to stretch".into()],
            Buttons{left: true, meta: false, ..} =>
                [(m + ": Moving").into(), "Release to stop".into()],
            Buttons{left: true, meta: true, ..} =>
                [(m + ": Stretching").into(), "Release to stop".into()],
        };
        ctx.emit_event(AppEvent::SetHint(m, a))
    }

    #[inline] fn on_selection_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        cursor: Cursor,
        first: bool
    ) {
        if !first && editor.last_cursor().is_some_and(|x| x.left == cursor.left) {return}
        let m = {let n = editor.selection().len(); format!("{n} note{}", if n == 1 {""} else {"s"})};
        let [m, a] = match *cursor {
            Buttons{left: false, ..} =>
                [m.into(), "LMB to move, LMB + Meta to stretch".into()],
            Buttons{left: true, meta: false, ..} =>
                [(m + ": Moving").into(), "Release to stop".into()],
            Buttons{left: true, meta: true, ..} =>
                [(m + ": Stretching").into(), "Release to stop".into()],
        };
        ctx.emit_event(AppEvent::SetHint(m, a))
    }

    #[inline] fn on_undo(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        action: &AppAction
    ) {
        match action {
            AppAction::AddNoteBlock{block_id, ..} => {
                editor.remove_points(once(*block_id), drop);
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
                editor.expand_selection(removed.iter().map(|(id, _)| *id));
            }

            _ => (),
        }
    }

    #[inline] fn on_redo(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        action: &AppAction
    ) {
        match action {
            &AppAction::AddNoteBlock{offset, value, block_id} => {
                unsafe{editor.insert_point(block_id, NoteBlock{offset, value, len: r64![1.0]})};
                ctx.emit_event(AppEvent::RedrawEditorPlane)
            }

            AppAction::RemoveNoteBlocks(blocks) => {
                editor.remove_points(blocks.iter().map(|(id, _)| *id), drop);
                ctx.emit_event(AppEvent::RedrawEditorPlane)
            }

            &AppAction::StretchNoteBlocks{delta_x, delta_y, ref removed} => {
                if editor.remove_points(removed.iter().map(|(id, _)| *id), drop).is_none() {return}
                for mut b in editor.iter_selection_mut() {
                    *b.inner() += delta_x;
                    *b.y() -= delta_y;
                }
            }
            _ => (),
        }
    }

    fn on_redraw(
        editor:      &mut GraphEditor<Self>,
        ctx:         &mut AppContext,
        canvas_size: &[R64; 2],
        solid:       &Path2d,
        _:           &Path2d,
        n_reps:      Self::VisualContext
    ) {
        let step = &canvas_size.div(&editor.scale());
        let offset = &R64::array_from(editor.offset());
        for block in editor.iter() {
            let [x, y] = block.loc().mul(step).sub(offset);
            solid.rect(*x, *y, *block.len * *step[0], *step[1]);
        }

        let play_since = ctx.play_since();
        let total_len = editor.last().map_or_default(|x| x.offset + x.len);
        let x = (ctx.now() - play_since).secs_to_beats(ctx.bps());
        if x < total_len * n_reps {
            editor.force_redraw();
            let x = R64::new_or(x, *x % *total_len) * step[0] - offset[0];
            solid.move_to(*x, 0.0);
            solid.line_to(*x, *canvas_size[1]);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CustomBlock {
    offset: R64,
    pitch: Note
}

impl PartialOrd for CustomBlock {
    #[inline] fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.offset.cmp(&other.offset))
    }
}

impl Ord for CustomBlock {
    #[inline] fn cmp(&self, other: &Self) -> Ordering {
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
    /// (number of repetitions of the pattern, audio duration)
    type VisualContext = (NonZeroUsize, R64);

    #[inline] fn inner(&self) -> &Self::Inner {&()}
    #[inline] fn inner_mut(&mut self) -> &mut Self::Inner {unsafe{transmute(self)}}

    #[inline] fn y(&self) -> &Self::Y {&self.pitch}
    #[inline] fn y_mut(&mut self) -> &mut Self::Y {&mut self.pitch}

    #[inline] fn loc(&self) -> [R64; 2] {[self.offset, self.pitch.recip().index().into()]}

    fn move_point(point: Result<&mut Self, &mut [R64; 2]>, delta: [R64; 2], _: bool) {
        match point {
            Ok(p) => {
                p.offset += delta[0];
                p.pitch += delta[1].into();
            }
            Err(p) => *p = p.add(&delta)
        }
    }

    #[inline] fn in_hitbox(&self, point: [R64; 2], (_, len): Self::VisualContext) -> bool {
        self.pitch.recip().index() == *point[1] as usize
            && (self.offset .. self.offset + len).contains(&point[0])
    }

    #[inline] fn fmt_loc(loc: [R64; 2]) -> String {
        format!("{:.3}, {}", loc[0], Note::from_index(loc[1].into()).recip())
    }

    #[inline] fn on_move(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        _: Cursor,
        _: [R64; 2],
        point: Option<usize>
    ) {
        let last = editor.len() - 1;
        if point.map_or_else(|| editor.selection().contains(&last), |x| x == last) {
            ctx.emit_event(AppEvent::RedrawEditorPlane)
        }
    }

    fn on_click(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        cursor: Cursor,
        pressed_at:    impl Deref<Target = [R64; 2]>,
        released_at:   impl Deref<Target = [R64; 2]>,
        old_selection: Option<&[usize]>
    ) -> Option<AppAction> {
        if !cursor.meta {return None}

        let delta = released_at.sub(&pressed_at);
        if delta.all(|x| *x == 0) {
            if !editor.selection().is_empty() || old_selection.map_or(false, |x| !x.is_empty()) {return None}

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
        }
    }

    #[inline] fn on_plane_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        cursor: Cursor,
        _: impl Deref<Target = [R64; 2]>,
        first: bool
    ) {
        if !first && editor.last_cursor().is_some_and(|x| *x == *cursor) {return}
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
        ctx.emit_event(AppEvent::SetHint(m, a))
    }

    #[inline] fn on_point_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        cursor: Cursor,
        point_id: usize,
        first: bool
    ) {
        if !first && editor.last_cursor().is_some_and(|x| x.left == cursor.left) {return}
        let m = Self::fmt_loc(unsafe{editor.get_unchecked(point_id)}.loc()).into();
        let [m, a] = match *cursor {
            Buttons{left: false, ..} =>
                [m, "LMB to move, LMB + Meta to remove".into()],
            Buttons{left: true, meta: false, ..} =>
                [m + ": Moving", "Release to stop".into()],
            Buttons{left: true, meta: true, ..} =>
                [m + ": Removing", "Release to remove".into()],
        };
        ctx.emit_event(AppEvent::SetHint(m, a))
    }

    #[inline] fn on_selection_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        cursor: Cursor,
        first: bool
    ) {
        if !first && editor.last_cursor().is_some_and(|x| x.left == cursor.left) {return}
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
        ctx.emit_event(AppEvent::SetHint(m, a))
    }

    #[inline] fn on_undo(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        action: &AppAction
    ) {
        match action {
            &AppAction::AddCustomBlock(at, _) => {
                editor.remove_points(once(at), drop);
                ctx.emit_event(AppEvent::RedrawEditorPlane)
            }

            AppAction::RemoveCustomBlocks(blocks) => {
                for &(at, b) in blocks.as_ref() {
                    unsafe{editor.insert_point(at, b)}
                }
                ctx.emit_event(AppEvent::RedrawEditorPlane)
            }

            _ => (),
        }
    }

    #[inline] fn on_redo(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        action: &AppAction
    ) {
        match action {
            &AppAction::AddCustomBlock(at, block) => {
                unsafe{editor.insert_point(at, block)};
                ctx.emit_event(AppEvent::RedrawEditorPlane)
            }

            AppAction::RemoveCustomBlocks(removed) => {
                editor.remove_points(removed.iter().map(|(id, _)| *id), drop);
                ctx.emit_event(AppEvent::RedrawEditorPlane)
            }

            _ => (),
        }
    }

    fn on_redraw(
        editor:        &mut GraphEditor<Self>,
        ctx:           &mut AppContext,
        canvas_size:   &[R64; 2],
        solid:         &Path2d,
        _:             &Path2d,
        (n_reps, len): Self::VisualContext
    ) {
        let step = &canvas_size.div(&editor.scale());
        let offset = &R64::array_from(editor.offset());
        for block in editor.iter() {
            let [x, y] = block.loc().mul(step).sub(offset);
            solid.rect(*x, *y, *len * *step[0], *step[1]);
        }

        let play_since = ctx.play_since();
        let total_len = editor.last().map_or_default(|x| x.offset + len);
        let x = (ctx.now() - play_since).secs_to_beats(ctx.bps());
        if x < total_len * n_reps {
            editor.force_redraw();
            let x = R64::new_or(x, *x % *total_len) * step[0] - offset[0];
            solid.move_to(*x, 0.0);
            solid.line_to(*x, *canvas_size[1]);
        }
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
    Custom{pattern: Shared<GraphEditor<CustomBlock>>, src: AudioBuffer,
        volume: R32, attack: Beats, decay: Beats, sustain: R32, release: Beats,
        rep_count: NonZeroUsize, speed: R32}
}

impl Sound {
    pub const TYPES: [SoundType; variant_count::<SoundType>()] = [
        SoundType::Note,
        SoundType::Noise,
        SoundType::Custom
    ];

    #[inline] pub fn new(sound_type: SoundType, ctx: &AudioContext) -> Option<Self> {
        Some(match sound_type {
            SoundType::Note =>
                Self::Note{pattern: default(),
                    volume: r32![1.0], attack: r64![0.0], decay: r64![0.0], sustain: r32![1.0], release: r64![0.2],
                    rep_count: NonZeroUsize::MIN},

            SoundType::Noise => {
                let len = ctx.sample_rate();
                let mut src_buf = vec![0.0f32; len as usize];
                src_buf.fill_with(|| random() as f32 * 2.0 - 1.0);
                let src = ctx.create_buffer(2, len as u32, len).report()?;
                src.copy_to_channel(&src_buf, 0).report()?;
                src.copy_to_channel(&src_buf, 1).report()?;
                Self::Noise{pattern: default(), src,
                    volume: r32![0.2], attack: r64![0.0], decay: r64![0.0], sustain: r32![1.0], release: r64![0.2],
                    rep_count: NonZeroUsize::MIN}
            }

            SoundType::Custom => {
                let sample_rate = ctx.sample_rate();
                let src = ctx.create_buffer(2, 1, sample_rate).report()?;
                Self::Custom{pattern: default(), src,
                    volume: r32![1.0], attack: r64![0.0], decay: r64![0.0], sustain: r32![1.0], release: r64![0.0],
                    rep_count: NonZeroUsize::MIN, speed: r32![1.0]}
            }
        })
    }

    #[inline] pub fn name(&self) -> &'static str {
        match self {
            Self::None => "Undefined",
            Self::Note{..} => "Simple Wave",
            Self::Noise{..} => "White Noise",
            Self::Custom{..} => "Custom"
        }
    }

    /// called before starting to play the sounds to reset their state and allow them to schedule
    /// their starting events
    pub fn reset(&mut self, _: &AppContext, self_id: usize, self_offset: Beats, mut scheduler: impl FnMut(SoundEvent))
    -> Option<()> {
        Some(match *self {
            Self::None => (),

            Self::Note{ref pattern, rep_count, ..} | Self::Noise{ref pattern, rep_count, ..} => {
                let p = pattern.try_borrow().report_fmt()?;
                let Some(base) = p.first().map(|x| x.offset + self_offset) else {return Some(())};
                let len = self.len();
                for i in 0 .. rep_count.get() {
                    scheduler(SoundEvent::BlockStart{id: self_id, state: 0, when: base + len * i})
                }
            }

            Self::Custom{ref pattern, rep_count, ..} => {
                let p = pattern.try_borrow().report_fmt()?;
                let Some(base) = p.first().map(|x| x.offset + self_offset) else {return Some(())};
                let len = self.len();
                for i in 0 .. rep_count.get() {
                    scheduler(SoundEvent::BlockStart{id: self_id, state: 0, when: base + len * i})
                }
            }
        })
    }

    pub fn poll(&mut self, plug: &AudioNode, ctx: &AppContext, event: SoundEvent, mut scheduler: impl FnMut(SoundEvent))
    -> Option<()> {
        Some(match *self {
            Self::None => (),

            Self::Note{volume, ref pattern, attack, decay, mut sustain, release, ..} => match event {
                SoundEvent::BlockStart{id, when, mut state} => {
                    let pattern = pattern.try_borrow().report_fmt()?;
                    let cur = unsafe{pattern.get_unchecked(state)};
                    let block_core = ctx.audio_ctx().create_oscillator().report()?;
                    let block = ctx.audio_ctx().create_gain().report()?;
                    block_core.frequency().set_value(*cur.value.freq());
                    block_core.start().report()?;
                    block_core.connect_with_audio_node(&block).report()?
                        .connect_with_audio_node(plug).report()?;

                    let now = ctx.now();
                    let mut at = now;
                    let gain = block.gain();
                    let bps = ctx.bps();
                    gain.set_value_at_time(f32::MIN_POSITIVE, *at).report()?;
                    at += attack.to_secs(bps);
                    gain.linear_ramp_to_value_at_time(*volume, *at).report()?;
                    at += decay.to_secs(bps);
                    sustain *= volume;
                    gain.linear_ramp_to_value_at_time(*sustain, *at).report()?;
                    at = now + cur.len.to_secs(bps);
                    gain.set_value_at_time(*sustain, *at).report()?;
                    at += release.to_secs(bps);
                    gain.linear_ramp_to_value_at_time(f32::MIN_POSITIVE, *at).report()?;

                    scheduler(SoundEvent::BlockEnd{id, when: when + cur.len + *release + r64![0.1].secs_to_beats(bps), block});

                    state += 1;
                    if let Some(next) = pattern.get(state) {
                        scheduler(SoundEvent::BlockStart{id, when: when + next.offset - cur.offset, state})
                    }
                }

                SoundEvent::BlockEnd{block, ..} => block.disconnect().report()?,
            }

            Self::Noise{ref pattern, ref src, volume, attack, decay, mut sustain, release, ..} => match event {
                SoundEvent::BlockStart{id, when, mut state} => {
                    let pattern = pattern.try_borrow().report_fmt()?;
                    let cur = unsafe{pattern.get_unchecked(state)};
                    let block_core = ctx.audio_ctx().create_buffer_source().report()?;
                    let block = ctx.audio_ctx().create_gain().report()?;
                    block_core.set_buffer(Some(src));
                    block_core.set_loop(true);
                    block_core.start().report()?;
                    block_core.connect_with_audio_node(&block).report()?
                        .connect_with_audio_node(plug).report()?;
                    js_log!("eee");

                    let bps = ctx.bps();
                    let now = when.to_secs(bps) + ctx.play_since();
                    let mut at = now;
                    let gain = block.gain();
                    gain.set_value(0.0);
                    gain.set_value_at_time(0.0, *at).report()?;
                    at += attack.to_secs(bps);
                    gain.linear_ramp_to_value_at_time(*volume, *at).report()?;
                    at += decay.to_secs(bps);
                    sustain *= volume;
                    gain.linear_ramp_to_value_at_time(*sustain, *at).report()?;
                    at = now + cur.len.to_secs(bps);
                    gain.set_value_at_time(*sustain, *at).report()?;
                    at += release.to_secs(bps);
                    gain.linear_ramp_to_value_at_time(f32::MIN_POSITIVE, *at).report()?;

                    scheduler(SoundEvent::BlockEnd{id, when: when + cur.len + *release + r64![0.1].secs_to_beats(bps), block});

                    state += 1;
                    if let Some(next) = pattern.get(state) {
                        scheduler(SoundEvent::BlockStart{id, when: when + next.offset - cur.offset, state})
                    }
                }

                SoundEvent::BlockEnd{block, ..} => block.disconnect().report()?,
            }

            Self::Custom{ref pattern, ref src, volume, attack, decay, mut sustain, release, speed, ..} => match event {
                SoundEvent::BlockStart{id, when, mut state} => {
                    let bps = ctx.bps();
                    let now = ctx.now();
                    let len = Secs::try_from(src.duration()).report_fmt()? / speed;
                    let pattern = pattern.try_borrow().report_fmt()?;
                    let audio_ctx = ctx.audio_ctx();
                    let block_core = audio_ctx.create_constant_source().report()?;
                    let shaper = audio_ctx.create_wave_shaper().report()?;
                    let block = ctx.audio_ctx().create_gain().report()?;
                    shaper.set_curve(Some(&mut src.get_channel_data(0).report()?));
                    block_core.connect_with_audio_node(&shaper).report()?
                        .connect_with_audio_node(&block).report()?
                        .connect_with_audio_node(plug).report()?;

                    let mut at = now;
                    let gain = block.gain();
                    let offset = block_core.offset();
                    gain.set_value(0.0);
                    offset.set_value(-1.0);
                    gain.set_value_at_time(0.0, *at).report()?;
                    offset.set_value_at_time(-1.0, *at).report()?;
                    at += attack.to_secs(bps);
                    gain.linear_ramp_to_value_at_time(*volume, *at).report()?;
                    at += decay.to_secs(bps);
                    sustain *= volume;
                    gain.linear_ramp_to_value_at_time(*sustain, *at).report()?;
                    at = now + len;
                    gain.set_value_at_time(*sustain, *at).report()?;
                    offset.linear_ramp_to_value_at_time(1.0, *at).report()?;
                    at += release.to_secs(bps);
                    gain.linear_ramp_to_value_at_time(f32::MIN_POSITIVE, *at).report()?;
                    block_core.start().report()?;

                    scheduler(SoundEvent::BlockEnd{id, when: when + len.secs_to_beats(bps) + *release + 1, block});

                    state += 1;
                    if let Some(next) = pattern.get(state) {
                        scheduler(SoundEvent::BlockStart{id,
                            when: when + next.offset - unsafe{pattern.get_unchecked(state - 1)}.offset, state})
                    }
                }

                SoundEvent::BlockEnd{block, ..} => block.disconnect().report()?,
            }
        })
    }

    #[inline] pub fn len(&self) -> Beats {
        match self {
            Self::None => r64![1.0],

            Self::Note {pattern, ..} |
            Self::Noise{pattern, ..} => pattern.try_borrow().report_fmt()
                .map_or_default(|p| p.last().map_or_default(|x| x.offset + x.len)),

            Self::Custom{pattern, src, speed, ..} => pattern.try_borrow().report_fmt()
                .map_or_default(|p| p.last().map_or_default(|x| x.offset + src.duration() / **speed as f64))
        }
    }
    
    #[inline] pub fn rep_count(&self) -> NonZeroUsize {
        match *self {
            Self::None => NonZeroUsize::MIN,
            Self::Note  {rep_count, ..} |
            Self::Noise {rep_count, ..} |
            Self::Custom{rep_count, ..} => rep_count
        }
    }

    #[inline] pub fn tabs(&self) -> &'static [TabInfo] {
        match self {
            Self::None =>
                &[TabInfo{name: "Choose Sound Type"}],
            Self::Note{..} | Self::Noise{..} | Self::Custom{..} =>
                &[TabInfo{name: "General"}, TabInfo{name: "Envelope"}, TabInfo{name: "Pattern"}]
        }
    }

    pub fn params(&self, ctx: &AppContext, setter: Callback<AppEvent>) -> Html {
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
                    min={r64![1.0]}
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

            Self::Custom{pattern, volume, attack, decay, sustain, release, rep_count, speed, ..} => match ctx.selected_tab() {
                0 /* General */ => html!{<div id="inputs">
                    <Slider key="custom-vol"
                    setter={setter.reform(|x| AppEvent::Volume(R32::from(x)))}
                    name="Noise Volume"
                    initial={*volume}/>
                    <Counter key="custom-repcnt"
                    setter={setter.reform(|x| AppEvent::RepCount(NonZeroUsize::from(x)))}
                    fmt={|x| format!("{}", usize::from(x))}
                    name="Number Of Pattern Repetitions"
                    min={r64![1.0]}
                    initial={*rep_count}/>
                    <input type="file"
                    onchange={setter.reform(AppEvent::AudioUploaded)}/>
                    <Counter key="note-speed"
                    setter={setter.reform(|x| AppEvent::Speed(R32::from(x)))}
                    fmt={|x| format!("{x:.2}x")}
                    name="Playback speed"
                    initial={*speed}/>
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

    pub fn handle_event(&mut self, event: &AppEvent, ctx: &mut AppContext) -> Option<()> {
        Some(match self {
            Sound::None => match event {
                &AppEvent::SetBlockType(ty) => {
                    *self = Self::new(ty, ctx.audio_ctx())?;
                    ctx.register_action(AppAction::SetBlockType(ty));
                    ctx.emit_event(AppEvent::RedrawEditorPlane)
                }

                AppEvent::Redo(actions) => for action in actions.iter() {
                    if let &AppAction::SetBlockType(ty) = action {
                        *self = Self::new(ty, ctx.audio_ctx())?;
                        ctx.emit_event(AppEvent::RedrawEditorPlane)
                    }
                }

                _ => (),
            }

            Sound::Note {pattern, volume, attack, decay, sustain, release, rep_count} |
            Sound::Noise{pattern, volume, attack, decay, sustain, release, rep_count, ..} => match event {
                &AppEvent::Volume  (to) => ctx.register_action(AppAction::SetVolume  {from: replace(volume,    to), to}),
                &AppEvent::Attack  (to) => ctx.register_action(AppAction::SetAttack  {from: replace(attack,    to), to}),
                &AppEvent::Decay   (to) => ctx.register_action(AppAction::SetDecay   {from: replace(decay,     to), to}),
                &AppEvent::Sustain (to) => ctx.register_action(AppAction::SetSustain {from: replace(sustain,   to), to}),
                &AppEvent::Release (to) => ctx.register_action(AppAction::SetRelease {from: replace(release,   to), to}),
                &AppEvent::RepCount(to) => {
                    ctx.register_action(AppAction::SetRepCount{from: replace(rep_count, to), to});
                    ctx.emit_event(AppEvent::RedrawEditorPlane)
                }

                e => {
                    let pattern = if ctx.selected_tab() == 2 {
                        let mut p = pattern.try_borrow_mut().report_fmt()?;
                        p.handle_event(e, ctx, || *rep_count)?;
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

            Sound::Custom{pattern, src, volume, attack, decay, sustain, release, rep_count, speed, ..} => match event {
                &AppEvent::Volume  (to) => ctx.register_action(AppAction::SetVolume  {from: replace(volume,    to), to}),
                &AppEvent::Attack  (to) => ctx.register_action(AppAction::SetAttack  {from: replace(attack,    to), to}),
                &AppEvent::Decay   (to) => ctx.register_action(AppAction::SetDecay   {from: replace(decay,     to), to}),
                &AppEvent::Sustain (to) => ctx.register_action(AppAction::SetSustain {from: replace(sustain,   to), to}),
                &AppEvent::Release (to) => ctx.register_action(AppAction::SetRelease {from: replace(release,   to), to}),
                &AppEvent::RepCount(to) => {
                    ctx.register_action(AppAction::SetRepCount{from: replace(rep_count, to), to});
                    ctx.emit_event(AppEvent::RedrawEditorPlane)
                }
                &AppEvent::Speed(to) => {
                    ctx.register_action(AppAction::SetSpeed{from: replace(speed, to), to});
                    ctx.emit_event(AppEvent::RedrawEditorPlane)
                }

                AppEvent::AudioUploaded(e) => {
                    let target: HtmlInputElement = e.target_dyn_into().report_none()?;
                    let emitter = ctx.event_emitter().clone();
                    let ctx = Rc::clone(ctx.audio_ctx());

                    spawn_local(async move {
                        let _: Option<!> = try {
                            let file = target.files().report_none()?.get(0).report_none()?;
                            let buf = JsFuture::from(file.array_buffer())
                                .await.report()?
                                .dyn_into().report()?;
                            let buf = JsFuture::from(ctx.decode_audio_data(&buf).report()?)
                                .await.report()?
                                .dyn_into().report()?;
                            emitter.emit(AppEvent::AudioProcessed(buf));
                            return
                        };
                    })
                }

                AppEvent::AudioProcessed(buf) => {
                    *src = buf.clone();
                    ctx.emit_event(AppEvent::RedrawEditorPlane)
                }

                e => {
                    if ctx.selected_tab() == 2 {
                        pattern.try_borrow_mut().report_fmt()?
                            .handle_event(e, ctx, ||
                                (*rep_count, R64::new(src.duration() / **speed as f64).report_none().unwrap_or_default()))?;
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

#[derive(Debug, Clone)]
pub struct SoundBlock {
    pub sound: Sound,
    pub layer: i32,
    pub offset: Beats
}

impl Deref for SoundBlock {
    type Target = Sound;
    #[inline] fn deref(&self) -> &Self::Target {&self.sound}
}

impl DerefMut for SoundBlock {
    #[inline] fn deref_mut(&mut self) -> &mut Self::Target {&mut self.sound}
}

impl PartialEq for SoundBlock {
    #[inline] fn eq(&self, other: &Self) -> bool {
        self.offset.eq(&other.offset)
    }
}

impl Eq for SoundBlock {}

impl PartialOrd for SoundBlock {
    #[inline] fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.offset.cmp(&other.offset))
    }
}

impl Ord for SoundBlock {
    #[inline] fn cmp(&self, other: &Self) -> Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl GraphPoint for SoundBlock {
    const EDITOR_NAME: &'static str = "Editor plane";
    const Y_BOUND: Range<R64> = r64![0.0] .. R64::INFINITY;
    const SCALE_Y_BOUND: Range<R64> = r64![5.0] .. r64![30.0];
    const OFFSET_Y_BOUND: Range<R64> = r64![-1.0] .. R64::INFINITY;
    const Y_SNAP: R64 = r64![1.0];
    type Inner = Sound;
    type Y = i32;
    type VisualContext = ();

    #[inline] fn inner(&self) -> &Self::Inner {&self.sound}
    #[inline] fn inner_mut(&mut self) -> &mut Self::Inner {&mut self.sound}

    #[inline] fn y(&self) -> &Self::Y {&self.layer}
    #[inline] fn y_mut(&mut self) -> &mut Self::Y {&mut self.layer}

    #[inline] fn loc(&self) -> [R64; 2] {[self.offset, self.layer.into()]}

    #[inline] fn move_point(point: Result<&mut Self, &mut [R64; 2]>, delta: [R64; 2], _: bool) {
         match point {
            Ok(SoundBlock{layer, offset, ..}) => {
                *offset = r64![0.0].max(*offset + delta[0]);
                *layer += i32::from(delta[1]);
            }
            Err(point) => {
                point[0] = r64![0.0].max(point[0] + delta[0]);
                point[1] += delta[1];
            }
        }
    }

    #[inline] fn in_hitbox(&self, point: [R64; 2], _: Self::VisualContext) -> bool {
        self.layer == *point[1] as i32
            && (self.offset .. self.offset + self.sound.len().max(r64![0.1]))
                .contains(&point[0])
    }

    #[inline] fn fmt_loc(loc: [R64; 2]) -> String {
        format!("{:.3}, layer {}", loc[0], loc[1].floor())
    }

    #[inline] fn on_click(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        cursor: Cursor,
        pressed_at:    impl Deref<Target = [R64; 2]>,
        released_at:   impl Deref<Target = [R64; 2]>,
        old_selection: Option<&[usize]>
    ) -> Option<AppAction> {
        let sel_is_empty = LazyCell::new(|| editor.selection().is_empty());
        if cursor.meta && *sel_is_empty && old_selection.map_or(true, |x| x.is_empty()) && *pressed_at == *released_at {
            let [offset, y] = *released_at;
            let layer = y.into();
            let block_id = editor.add_point(SoundBlock{sound: default(), layer, offset});
            Some(AppAction::AddSoundBlock{block_id, offset, layer})
        } else {
            ctx.emit_event(AppEvent::Select(sel_is_empty.not().then_some(0)));
            None
        }
    }

    #[inline] fn on_plane_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        cursor: Cursor,
        _: impl Deref<Target = [R64; 2]>,
        first: bool
    ) {
        if !first && Some(&*cursor) == editor.last_cursor().as_deref() {return}
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
                "Click to add block, hold & drag to select".into()],
            Buttons{left: true, meta: false, ..} => 
                [Cow::from(Self::EDITOR_NAME) + ": Moving",
                "Release to stop".into()],
            Buttons{left: true, meta: true, ..} =>
                [Cow::from(Self::EDITOR_NAME) + ": Selecting",
                "Release to select".into()]
        };
        ctx.emit_event(AppEvent::SetHint(m, a))
    }

    #[inline] fn on_point_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        cursor: Cursor,
        point_id: usize,
        first: bool
    ) {
        let m = || unsafe{editor.get_unchecked(point_id)}.desc().into();
        let [m, a] = if cursor.left {
            [m() + ": moving", "Release to stop".into()]
        } else if first {
            [m(), "Hold & drag to move".into()]
        } else {return};
        ctx.emit_event(AppEvent::SetHint(m, a))
    }

    #[inline] fn on_selection_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        cursor: Cursor,
        first: bool
    ) {
        if !first {return}
        let m = editor.selection().len().pipe(|l| format!("{l} block{}", if l == 1 {""} else {"s"}));
        let [m, a] = if cursor.left {
            [(m + ": moving").into(), "Release to stop".into()]
        } else {
            [m.into(), "Hold & drag to move".into()]
        };
        ctx.emit_event(AppEvent::SetHint(m, a))
    }

    #[inline] fn on_undo(
        editor: &mut GraphEditor<Self>,
        _: &mut AppContext,
        action: &AppAction
    ) {
        if let &AppAction::AddSoundBlock{block_id, ..} = action {
            editor.remove_points(once(block_id), drop);
        }
    }

    #[inline] fn on_redo(
        editor: &mut GraphEditor<Self>,
        _: &mut AppContext,
        action: &AppAction
    ) {
        if let &AppAction::AddSoundBlock{offset, layer, block_id} = action {
            unsafe{editor.insert_point(block_id, SoundBlock{sound: default(), layer, offset})}
        }
    }

    fn on_redraw(
        editor:      &mut GraphEditor<Self>,
        ctx:         &mut AppContext,
        canvas_size: &[R64; 2],
        solid:       &Path2d,
        dotted:      &Path2d,
        _:           Self::VisualContext
    ) {
        let step = &canvas_size.div(&editor.scale());
        let offset = &R64::array_from(editor.offset());
        for block in editor.iter() {
            let [mut x, y] = block.loc().mul(step).sub(offset).map(|x| *x);
            let n_reps = block.rep_count().get();
            let w = *block.len() * *step[0];
            solid.rect(x, y, w, *step[1]);
            for _ in 1 .. n_reps {
                x += w;
                dotted.rect(x, y, w, *step[1])
            }
        }

        let play_since = ctx.play_since();
        if play_since.is_finite() {
            editor.force_redraw();
            let x = (ctx.now() - play_since).secs_to_beats(ctx.bps()) * step[0] - offset[0];
            solid.move_to(*x, 0.0);
            solid.line_to(*x, *canvas_size[1]);
        }
    }

    #[inline] fn canvas_coords(canvas: &HtmlCanvasElement) -> JsResult<[u32; 2]> {
        let doc = document();
        let w = doc.body().to_js_result()?.client_width()
            - canvas.previous_element_sibling().to_js_result()?
            .client_width();
        let h = canvas.client_height();
        Ok([w as u32, h as u32])
    }
}

impl SoundBlock {
    #[inline] pub fn desc(&self) -> String {
        format!("{} @ {}", self.sound.name(), Self::fmt_loc(self.loc()))
    }
}

/// all the `when` fields are offsets from the start in beats
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SoundEvent {
    BlockStart{id: usize, when: Beats, state: usize},
    BlockEnd{id: usize, when: Beats, block: GainNode}
}

impl PartialOrd for SoundEvent {
    #[inline] fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.when().cmp(&other.when()))
    }
}

impl Ord for SoundEvent {
    #[inline] fn cmp(&self, other: &Self) -> Ordering {
        self.when().cmp(&other.when())
    }
}

impl SoundEvent {
    #[inline] fn target(&self) -> usize {
        match self {
            SoundEvent::BlockStart{id, ..}
            | SoundEvent::BlockEnd{id, ..} => *id
        }
    }

    #[inline] fn when(&self) -> Secs {
        match self {
            SoundEvent::BlockStart{when, ..} 
            | SoundEvent::BlockEnd{when, ..} => *when,
        }
    }
}

pub struct Sequencer {
    pattern: Shared<GraphEditor<SoundBlock>>,
    pending: Vec<SoundEvent>,
    plug: DynamicsCompressorNode,
    gain: GainNode,
    playing: bool,
    used_to_play: bool
}

impl Sequencer {
    #[inline] pub fn new(audio_ctx: &AudioContext, visualiser: Rc<AnalyserNode>) -> Option<Self> {
        let plug = DynamicsCompressorNode::new(audio_ctx).report()?;
        plug.ratio().set_value(20.0);
        plug.release().set_value(1.0);
        let gain = GainNode::new(audio_ctx).report()?;
        gain.gain().set_value(0.2);

        plug.connect_with_audio_node(&gain).report()?
            .connect_with_audio_node(&visualiser).report()?
            .connect_with_audio_node(&audio_ctx.destination()).report()?;

        Some(Self{plug, gain,
            pattern: Rc::new(RefCell::new(GraphEditor::new(vec![]))), pending: vec![],
            playing: false, used_to_play: false})
    }

    #[inline] pub fn gain(&self) -> R32 {
        R32::new_or(R32::ZERO, self.gain.gain().value())
    }

    #[inline] pub fn pattern(&self) -> &Shared<GraphEditor<SoundBlock>> {
        &self.pattern
    }

    pub fn handle_event(&mut self, event: &AppEvent, ctx: &mut AppContext) -> Option<()> {
        Some(match event {
            AppEvent::StartPlay => {
                self.pending.clear();
                let mut pattern = self.pattern.try_borrow_mut().report_fmt()?;
                for (id, mut block) in pattern.iter_mut().enumerate() {
                    let offset = block.offset;
                    block.inner().reset(ctx, id, offset,
                        |x| _ = self.pending.push_sorted(x))?;
                }
                self.playing = true;
            }

            AppEvent::StopPlay => {
                self.pending.clear();
                self.plug.disconnect().report()?;
                self.plug = ctx.audio_ctx().create_dynamics_compressor().report()?;
                self.plug.ratio().set_value(20.0);
                self.plug.release().set_value(1.0);
                self.plug.connect_with_audio_node(&self.gain).report()?;
                self.playing = false;
                self.used_to_play = false;
            }

            AppEvent::RedrawEditorPlane => self.pattern
                .try_borrow_mut().report_fmt()?.force_redraw(),

            &AppEvent::MasterVolume(to) => {
                ctx.register_action(AppAction::SetMasterVolume{from: self.gain(), to});
                self.gain.gain().set_value(*to)
            }

            AppEvent::Frame(_) => {
                let mut pattern = self.pattern.try_borrow_mut().report_fmt()?;
                if self.playing {
                    let (ctx, now) = if self.used_to_play {
                        (Cow::Borrowed(ctx), (ctx.now() - ctx.play_since()).secs_to_beats(ctx.bps()))
                    } else {
                        self.used_to_play = true;
                        pattern.force_redraw();
                        ctx.emit_event(AppEvent::AudioStarted(ctx.now()));
                        (Cow::Owned(ctx.clone().set_play_since(ctx.now())), r64![0.0])
                    };
                    let n_due = self.pending.iter().position(|x| x.when() > now).unwrap_or(self.pending.len());
                    for event in self.pending.drain(..n_due).collect::<Vec<_>>() {
                        let id = event.target();
                        let mut block = unsafe{pattern.get_unchecked_mut(id)};
                        let mut due_now = vec![event];

                        while !due_now.is_empty() {
                            for event in due_now.take() {
                                block.inner().poll(&self.plug, &ctx, event, |new| if new.when() > now {
                                    self.pending.push_sorted(new);
                                } else {
                                    due_now.push(new);
                                })?;
                            }
                        }
                    }
                }
                pattern.handle_event(event, ctx, || ())?
            }

            e => {
                self.pattern.try_borrow_mut().report_fmt()?.handle_event(e, ctx, || ())?;

                match e {
                    AppEvent::Undo(actions) => for action in actions.iter() {
                        if let AppAction::SetMasterVolume{from, ..} = *action {
                            self.gain.gain().set_value(*from)
                        }
                    }

                    AppEvent::Redo(actions) => for action in actions.iter() {
                        if let AppAction::SetMasterVolume{to, ..} = *action {
                            self.gain.gain().set_value(*to)
                        }
                    }

                    _ => ()
                }
            }
        })
    }
}
