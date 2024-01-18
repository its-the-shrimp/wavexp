use crate::{
    ctx::{AppEvent, ContextMut, ContextRef, EditorAction, RemovedPoint},
    input::{Buttons, Cursor},
    sequencer::Sequencer,
};
use std::{
    borrow::Cow,
    cell::{Cell, LazyCell},
    cmp::{min, Ordering},
    fmt::{self, Debug, Formatter},
    iter::{once, Iterator},
    mem::{replace, take},
    ops::{Deref, DerefMut, Range, RangeInclusive},
    rc::Rc,
    slice::from_raw_parts,
};
use wasm_bindgen::{Clamped, JsCast, JsValue};
use wavexp_utils::{
    cell::WasmCell, default, eval_once, iter::ToEveryNth, js_array, js_assert, r64, AppResult,
    AppResultUtils, ArrayExt, ArrayFrom, BoolExt, FlippedArray, HtmlCanvasExt, HtmlElementExt,
    IntoArray, OptionExt, Point, RangeExt, RoundTo, SliceExt, SliceRef, VecExt, R64,
};
use web_sys::{Element, HtmlCanvasElement, HtmlElement, ImageData, Path2d, SvgElement};
use yew::{NodeRef, TargetCast};

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Rgba {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl From<u32> for Rgba {
    fn from(val: u32) -> Self {
        Self {
            r: (val >> 24) as u8,
            g: ((val >> 16) & 0xFF) as u8,
            b: ((val >> 8) & 0xFF) as u8,
            a: (val & 0xFF) as u8,
        }
    }
}

impl From<Rgba> for u32 {
    fn from(val: Rgba) -> Self {
        val.a as u32 | (val.b as u32) << 8 | (val.g as u32) << 16 | (val.r as u32) << 24
    }
}

impl fmt::Display for Rgba {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "#{:02X}{:02X}{:02X}{:02X}",
            self.r, self.g, self.b, self.a
        )
    }
}

fn interp<const N: usize>(colours: &[Rgba; N], index: u8) -> Rgba {
    let index = (index as usize * (N - 1)) as f32 / 255.0;
    let lower = colours.get_saturating(index.floor() as usize);
    let upper = colours.get_saturating(index.ceil() as usize);
    let weight = (index / (N - 1) as f32).fract();
    let weight_recip = 1.0 - weight;
    Rgba {
        r: (lower.r as f32 * weight_recip + upper.r as f32 * weight) as u8,
        g: (lower.g as f32 * weight_recip + upper.g as f32 * weight) as u8,
        b: (lower.b as f32 * weight_recip + upper.b as f32 * weight) as u8,
        a: (lower.a as f32 * weight_recip + upper.a as f32 * weight) as u8,
    }
}

pub struct SoundVisualiser {
    out_data: Vec<Rgba>,
    in_data: Vec<u8>,
    gradient: Vec<Rgba>,
    canvas: NodeRef,
    width: u32,
    height: u32,
}

impl SoundVisualiser {
    pub const FG: Rgba = Rgba {
        r: 0x00,
        g: 0x69,
        b: 0xE1,
        a: 0xFF,
    };
    pub const BG: Rgba = Rgba {
        r: 0x18,
        g: 0x18,
        b: 0x18,
        a: 0xFF,
    };
    pub fn new() -> Self {
        Self {
            out_data: vec![],
            in_data: vec![],
            gradient: (0..=u8::MAX)
                .map(|i| interp(&[Self::BG, Self::FG], i))
                .collect(),
            width: 0,
            height: 0,
            canvas: default(),
        }
    }

    pub fn canvas(&self) -> &NodeRef {
        &self.canvas
    }

    // TODO: correctly readjust the graph when shrinked in the UI
    pub fn handle_event(&mut self, event: &AppEvent, sequencer: &Sequencer) -> AppResult<()> {
        Ok(match event {
            AppEvent::Resize => {
                let canvas: HtmlCanvasElement = self.canvas.cast().to_app_result()?;
                let [w, h] = canvas.client_size().map(|x| x as u32);
                canvas.set_width(w);
                canvas.set_height(h);
                self.width = w;
                self.height = h;
                self.in_data.resize(w as usize, 0);
                self.out_data.resize(w as usize * w as usize, Self::BG);
            }

            AppEvent::Frame(..) => {
                if sequencer.playback_ctx().playing() {
                    self.out_data.rotate_right(1);
                    sequencer
                        .analyser()
                        .get_byte_frequency_data(&mut self.in_data);
                    for (&src, dst) in self
                        .in_data
                        .iter()
                        .zip(self.out_data.every_nth_mut(self.width as usize))
                    {
                        *dst = unsafe { *self.gradient.get_unchecked(src as usize) };
                    }

                    let out = unsafe {
                        from_raw_parts(self.out_data.as_ptr().cast(), self.out_data.len() * 4)
                    };
                    let out = ImageData::new_with_u8_clamped_array(Clamped(out), self.width)?;
                    self.canvas
                        .cast::<HtmlCanvasElement>()
                        .to_app_result()?
                        .get_2d_context()?
                        .put_image_data(&out, 0.0, 0.0)?;
                }
            }

            _ => (),
        })
    }
}

#[derive(Debug, PartialEq, Default)]
pub struct HintHandler {
    main_bar: NodeRef,
    aux_bar: NodeRef,
}

impl HintHandler {
    pub fn handle_event(&mut self, event: &AppEvent) -> AppResult<()> {
        Ok(match event {
            AppEvent::SetHint(main, aux) => {
                self.main_bar
                    .cast::<HtmlElement>()
                    .to_app_result()?
                    .set_inner_text(main);
                self.aux_bar
                    .cast::<HtmlElement>()
                    .to_app_result()?
                    .set_inner_text(aux);
            }

            AppEvent::FetchHint(e) => {
                let main_bar: HtmlElement = self.main_bar.cast().to_app_result()?;
                let aux_bar: HtmlElement = self.aux_bar.cast().to_app_result()?;
                let mut src: Element = e.target_dyn_into().to_app_result()?;

                loop {
                    let dataset = if let Some(x) = src.dyn_ref::<HtmlElement>() {
                        x.dataset()
                    } else if let Some(x) = src.dyn_ref::<SvgElement>() {
                        x.dataset()
                    } else {
                        main_bar.set_inner_text("");
                        aux_bar.set_inner_text("");
                        break;
                    };
                    if let Some(main) = dataset.get("mainHint") {
                        main_bar.set_inner_text(&main);
                        aux_bar.set_inner_text(&dataset.get("auxHint").unwrap_or_default());
                        break;
                    }
                    if let Some(parent) = src.parent_element() {
                        src = parent
                    } else {
                        break default();
                    }
                }
            }

            _ => (),
        })
    }

    pub fn main_bar(&self) -> &NodeRef {
        &self.main_bar
    }

    pub fn aux_bar(&self) -> &NodeRef {
        &self.aux_bar
    }
}

/// data that can be edited with a generic graph editor defined below
pub trait GraphPoint: Sized + Clone + Ord + 'static {
    /// the name of the plane that will be displayed as a hint when hovered over it
    const EDITOR_NAME: &'static str;
    /// bounds for the points along the X axis
    const X_BOUND: Range<R64> = r64![0]..R64::INFINITY;
    /// bounds for the scale of the X axis of the graph
    const SCALE_X_BOUND: Range<R64> = r64![3]..r64![30];
    /// bounds for the offset of the X axis of the graph
    const OFFSET_X_BOUND: Range<R64> = r64![-1]..R64::INFINITY;
    /// bounds for the points along the Y axis
    const Y_BOUND: Range<R64>;
    /// bounds for the scale of the Y axis of the graph
    const SCALE_Y_BOUND: Range<R64>;
    /// bounds for the offset of the Y axis of the graph
    const OFFSET_Y_BOUND: Range<R64>;
    /// snap step along the Y axis (snap step along the X axis is set globally)
    const Y_SNAP: R64;
    /// type of the inner data of the point, can be whatever and,
    /// unlike the coordinates, can be freely mutated
    type Inner;
    /// type of the Y coordinate, used to allow modifying points' Y axis without unsafe code or
    /// additional checks, which would be needed to modify the points' X axis
    type Y;
    /// Type of the context passed to all the trait's functions related to visuals through `GraphEditor::handle_event()`.
    /// Such functions are `in_hitbox` and `on_redraw`.
    type VisualContext: Copy;

    /// Creates a new point from the given user coordinates.
    fn create(editor: &GraphEditor<Self>, at: [R64; 2]) -> Self;

    /// inner data of the point
    fn inner(&self) -> &Self::Inner;
    /// mutable inner data of the point
    fn inner_mut(&mut self) -> &mut Self::Inner;
    /// Y axis of the point
    fn y(&self) -> &Self::Y;
    /// mutable Y axis of the point
    fn y_mut(&mut self) -> &mut Self::Y;
    /// location of the point in user coordinates
    fn loc(&self) -> [R64; 2];
    /// change the location of the point in user coordinates when moved in the UI
    /// `meta` signifies whether the meta key was held while moving the point
    fn move_point(point: Result<&mut Self, &mut [R64; 2]>, delta: [R64; 2], meta: bool);
    /// returns `true` if the given `area` in user coordinates overlaps with the hitbox of the point.
    fn in_hitbox(
        &self,
        area: &[RangeInclusive<R64>; 2],
        ctx: ContextRef,
        sequencer: &Sequencer,
        visual_ctx: Self::VisualContext,
    ) -> AppResult<bool>;

    ////// HANDLERS
    /// Handle points being moved in the UI.
    /// `point` is the ID of the point that's being moved or `None` if a selection or plane is
    /// moved instead of 1 point.
    #[allow(unused_variables)]
    fn on_move(
        editor: &mut GraphEditor<Self>,
        ctx: ContextMut,
        cursor: Cursor,
        delta: [R64; 2],
        point: Option<usize>,
    ) -> AppResult<()> {
        Ok(())
    }

    /// Handle request for a redraw.
    /// `editor` is the editor that needs redraw.
    /// `ctx` is the application context.
    /// `sequencer` is the the app's global sequencer.
    /// `canvas_size` is canvas's dimensions in pixels.
    /// Points on a canvas must be confined to these dimensions to appear on the screen.
    /// `solid` is the path that will be stroked with a solid line and filled with light background color.
    /// `dotted` is the path that will be stroked with a dotted line.
    /// `visual_ctx` is the visual context defined for the graph point, passed to this handler
    /// through `GraphEditor::handle_event`.
    fn on_redraw(
        editor: &mut GraphEditor<Self>,
        ctx: ContextRef,
        sequencer: &Sequencer,
        canvas_size: &[R64; 2],
        solid: &Path2d,
        dotted: &Path2d,
        visual_ctx: Self::VisualContext,
    ) -> AppResult<()>;

    /// Handle change of selection area.
    /// `editor` is the editor, the selection area of which was changed.
    /// `app` is the application context.
    /// The current selection can be obtained through `editor.selection()`.
    #[allow(unused_variables)]
    fn on_selection_change(editor: &mut GraphEditor<Self>, ctx: ContextMut) -> AppResult<()> {
        Ok(())
    }

    /// `loc` is in user coordinates
    fn fmt_loc(loc: [R64; 2]) -> String;
    /// the canvas's coordinate space
    fn canvas_coords(canvas: &HtmlCanvasElement) -> AppResult<[u32; 2]> {
        Ok([canvas.client_width() as u32, canvas.client_height() as u32])
    }
}

/// a special reference wrapper: access to everything is mutable, except for the X axis
pub struct GraphPointView<'point, T: GraphPoint>(&'point mut T);

impl<'point, T: GraphPoint> Deref for GraphPointView<'point, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'point, T: GraphPoint> GraphPointView<'point, T> {
    pub fn inner(&mut self) -> &mut T::Inner {
        self.0.inner_mut()
    }
    //pub fn y(&mut self) -> &mut T::Y {self.0.y_mut()}

    // /// the caller must ensure that the point retains its sorted placement
    // pub unsafe fn unlock(self) -> &'a mut T {self.0}
}

// types as coordinate space hints:
/// in canvas coordinates with plane offset
type OffsetCanvasPoint = Point;
/// in user coordinates, aligned to snap step and limited to bounds specified by `GraphPoint::(X/Y)_BOUND`
type ConfinedAlignedUserPoint = [R64; 2];

/// action for a graph editor to perform when Meta key is pressed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SpecialAction {
    /// select points
    #[default]
    Select,
    /// add points
    Add,
    /// remove points
    Remove,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
enum Focus {
    #[default]
    None,
    Zoom {
        init_offset: Point,
        pivot: OffsetCanvasPoint,
        init_scale: [R64; 2],
    },
    // TODO: add `meta` here to ignore meta key held down after the left mouse button
    Plane {
        origin: ConfinedAlignedUserPoint,
        init_offset: Point,
    },
    Point {
        id: usize,
        last_loc: ConfinedAlignedUserPoint,
        origin: ConfinedAlignedUserPoint,
        meta: bool,
    },
    Selection {
        origin: ConfinedAlignedUserPoint,
        end: ConfinedAlignedUserPoint,
        meta: bool,
    },
}

static GRAPH_EDITOR_COUNT: WasmCell<Cell<usize>> =
    WasmCell(Cell::new(AnyGraphEditor::INVALID_ID + 1));

/// base of `GraphEditor`, distinguished from the former to ease working with cases when the
/// contained point type doesn't matter
#[derive(Debug, Clone, PartialEq, Default)]
pub struct AnyGraphEditor {
    canvas: NodeRef,
    offset: Point,
    scale: [R64; 2],
    focus: Focus,
    selection: Vec<usize>,
    selection_src: ConfinedAlignedUserPoint,
    selection_size: [R64; 2],
    last_cursor: Cursor,
    redraw: bool,
    update_hint: bool,
    grid: Option<(Path2d, [R64; 2])>,
    id: usize,
}

impl AnyGraphEditor {
    const FONT: &'static str = "20px consolas";
    const BG_STYLE: &'static str = "#232328";
    const MG_STYLE: &'static str = "#333338";
    const FG_STYLE: &'static str = "#0069E1";
    const LINE_WIDTH: f64 = 3.0;
    /// an ID that's guaranteed to never be used by any graph editor
    pub const INVALID_ID: usize = 0;

    pub fn id(&self) -> usize {
        self.id
    }
    pub fn canvas(&self) -> &NodeRef {
        &self.canvas
    }
    pub fn selection(&self) -> &[usize] {
        &self.selection
    }
    /// Offsets which must be subtracted from a canvas coordinate point for
    /// it to be correctly displayed to the user.
    /// Changed by the user dragging the editor plane.
    pub fn offset(&self) -> Point {
        self.offset
    }
    /// Coefficients by which a user coordinate point must be
    /// multiplied to get a canvas coordinate point.
    pub fn scale(&self) -> [R64; 2] {
        self.scale
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GraphEditor<T: GraphPoint> {
    inner: AnyGraphEditor,
    data: Vec<T>,
}

impl<T: GraphPoint> Deref for GraphEditor<T> {
    type Target = AnyGraphEditor;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T: GraphPoint> DerefMut for GraphEditor<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T: GraphPoint> Default for GraphEditor<T> {
    fn default() -> Self {
        Self::new(vec![])
    }
}

impl<T: GraphPoint> GraphEditor<T> {
    pub fn new(data: Vec<T>) -> Self {
        let res = Self {
            data,
            inner: AnyGraphEditor {
                scale: [T::SCALE_X_BOUND, T::SCALE_Y_BOUND]
                    .map(|x| x.to_pair().mul(&[0.75, 0.25]).sum()),
                id: GRAPH_EDITOR_COUNT.get(),
                ..default()
            },
        };
        GRAPH_EDITOR_COUNT.set(res.id + 1);
        res
    }

    pub fn data(&self) -> &[T] {
        &self.data
    }

    pub fn get_mut(&mut self, index: usize) -> Option<GraphPointView<'_, T>> {
        self.data.get_mut(index).map(GraphPointView)
    }

    pub fn iter_data_mut(&mut self) -> impl Iterator<Item = GraphPointView<'_, T>> {
        self.data.iter_mut().map(GraphPointView)
    }

    /// `to_remove` accepts an ID of the selected point and a reference to it,
    /// and returns `true` if the point stays or `false` if the point must be removed.
    /// `sink` is the function that will be called on every removed point.
    /// If there's nothing to do with the points, standard library's `drop` function can be passed.
    pub fn filter_selected(
        &mut self,
        mut to_remove: impl FnMut((usize, &T)) -> bool,
    ) -> EditorAction {
        let mut removed = vec![];
        for i in (0..self.selection.len()).rev() {
            unsafe {
                let index = *self.selection.get_unchecked(i);
                if !to_remove((index, self.data.get_unchecked(index))) {
                    let point = Rc::new(self.data.remove_unchecked(index));
                    removed.push(RemovedPoint {
                        point,
                        index,
                        was_selected: true,
                    });
                    self.selection.remove_unchecked(i);
                    self.redraw = true;
                }
            }
        }
        EditorAction::RemovePoint(self.id, removed.into_boxed_slice())
    }

    /// `to_remove` iterates over IDs of points that must be removed.
    /// Returns the action that represents the removal of the points.
    // TODO: make it adjust the selection
    pub fn remove_points(
        &mut self,
        to_remove: impl Iterator<Item = usize> + DoubleEndedIterator,
    ) -> AppResult<EditorAction> {
        self.redraw = true;
        let GraphEditor { inner, data } = self;
        let mut ids_iter = inner.selection.iter_mut().rev();
        let mut prev_id = data.len();
        let mut removed = vec![];
        for index in to_remove.rev() {
            js_assert!(index < replace(&mut prev_id, index))?;
            let (rem, was_selected) = loop {
                let Some(x) = ids_iter.next() else {
                    break (0, false);
                };
                match index.cmp(x) {
                    Ordering::Less => *x -= 1,
                    Ordering::Equal => {
                        break unsafe {
                            let x = ids_iter.len();
                            inner.selection.remove_unchecked(x);
                            (x, true)
                        }
                    }
                    Ordering::Greater => break (ids_iter.len(), false),
                }
            };
            removed.push(RemovedPoint {
                point: Rc::new(data.try_remove(index)?),
                index,
                was_selected,
            });
            ids_iter = inner
                .selection
                .get_mut(..rem)
                .unwrap_or(&mut [])
                .iter_mut()
                .rev();
        }
        Ok(EditorAction::RemovePoint(
            self.id,
            removed.into_boxed_slice(),
        ))
    }

    pub fn force_redraw(&mut self) {
        self.redraw = true
    }

    /// must be called when a canvas has just been bound or its dimensions have been changed
    pub fn init(&mut self) -> AppResult<()> {
        let canvas: HtmlCanvasElement = self.canvas.cast().to_app_result()?;
        let [w, h] = T::canvas_coords(&canvas)?;
        canvas.set_width(w);
        canvas.set_height(h);
        self.scale = self.scale.map(|x| x.ceil_to(r64![2]));
        self.grid = None;
        if self.offset.x <= 0 {
            self.offset.x = (T::OFFSET_X_BOUND.start * w / self.scale[0]).into();
        }
        if self.offset.y <= 0 {
            self.offset.y = (T::OFFSET_Y_BOUND.start * h / self.scale[1]).into();
        }
        let ctx = canvas.get_2d_context()?;
        ctx.set_font(AnyGraphEditor::FONT);
        ctx.set_line_width(AnyGraphEditor::LINE_WIDTH);
        Ok(self.redraw = true)
    }

    fn point_by_pos(
        &self,
        loc: [R64; 2],
        ctx: ContextMut,
        sequencer: &Sequencer,
        visual_ctx: T::VisualContext,
    ) -> AppResult<Option<SliceRef<'_, T>>> {
        Ok(self
            .data
            .iter()
            .enumerate()
            .try_find(|x| {
                x.1.in_hitbox(&loc.map(|x| x..=x), ctx.as_ref(), sequencer, visual_ctx)
            })?
            .map(|(id, x)| unsafe { SliceRef::raw(x, id) }))
    }

    fn point_in_selection(&self, loc: ConfinedAlignedUserPoint) -> bool {
        (r64![0]..=self.selection_size[0]).contains(&(loc[0] - self.selection_src[0]))
            && (r64![0]..=self.selection_size[1]).contains(&(loc[1] - self.selection_src[1]))
        /*loc.sub(&self.selection_src)
        .zip_fold(true, self.selection_size, |r, x, y| r && 0.0 <= *x && x <= y)*/
    }

    fn update_hint(&self, ctx: ContextMut, cursor: Cursor) {
        let (main, aux) = match self.focus {
            Focus::None => return,

            Focus::Zoom { .. } => (
                Cow::from(T::EDITOR_NAME) + ": zooming",
                if cursor.left {
                    "Release to stop"
                } else {
                    "Press and hold left mouse button to zoom"
                },
            ),

            Focus::Plane { .. } => {
                let main = Cow::from(T::EDITOR_NAME);
                match *cursor {
                    Buttons {
                        left: false,
                        meta: false,
                        ..
                    } => (
                        main,
                        match ctx.special_action() {
                            SpecialAction::Select => "Shift - zoom, Meta - select points",
                            SpecialAction::Add => "Shift - zoom, Meta - add a point",
                            SpecialAction::Remove => "Shift - zoom, Meta - remove points",
                        },
                    ),

                    Buttons {
                        left: false,
                        meta: true,
                        ..
                    } => match ctx.special_action() {
                        // TODO: find a way to replace `format!` with smth const
                        SpecialAction::Select => (
                            main + ": selecting",
                            "Press and hold left mouse button to select",
                        ),
                        SpecialAction::Add => (main + ": adding", "Click to add a point"),
                        SpecialAction::Remove => {
                            (main + ": removing", "Click on a point to remove it")
                        }
                    },

                    Buttons {
                        left: true,
                        meta: false,
                        ..
                    } => (main + ": moving", "Release to stop"),

                    Buttons {
                        left: true,
                        meta: true,
                        ..
                    } => match ctx.special_action() {
                        SpecialAction::Select => (main + ": selecting", "Release to select"),
                        SpecialAction::Add => (main + ": adding a point", "Release to add a point"),
                        SpecialAction::Remove => (
                            main + ": removing a point",
                            "Release and click on a point to remove it",
                        ),
                    },
                }
            }

            Focus::Point { id, .. } => {
                let main = || Cow::from(T::fmt_loc(unsafe { self.data.get_unchecked(id) }.loc()));
                match *cursor {
                    Buttons {
                        left: false,
                        meta: false,
                        ..
                    } => (
                        main(),
                        match ctx.special_action() {
                            SpecialAction::Select => "Shift - zoom, Meta - select points",
                            SpecialAction::Add => "Shift - zoom, Meta - add a point",
                            SpecialAction::Remove => "Shift - zoom, Meta - remove points",
                        },
                    ),

                    Buttons {
                        left: false,
                        meta: true,
                        ..
                    } => match ctx.special_action() {
                        // TODO: find a way to replace `format!` with smth const
                        SpecialAction::Select => (
                            Cow::from(T::EDITOR_NAME) + ": selecting",
                            "Press and hold left mouse button to select",
                        ),
                        SpecialAction::Add => (
                            Cow::from(T::EDITOR_NAME) + ": adding a point",
                            "Click on empty space to add a point",
                        ),
                        SpecialAction::Remove => {
                            (main() + ": removing a point", "Click to remove this point")
                        }
                    },

                    Buttons {
                        left: true,
                        meta: false,
                        ..
                    } => (main() + ": moving", "Release to stop"),

                    Buttons {
                        left: true,
                        meta: true,
                        ..
                    } => match ctx.special_action() {
                        SpecialAction::Select => (
                            Cow::from(T::EDITOR_NAME) + ": selecting",
                            "Release to select",
                        ),
                        SpecialAction::Add => (
                            Cow::from(T::EDITOR_NAME) + ": adding a point",
                            "Release and click on empty space to add a point",
                        ),
                        SpecialAction::Remove => (
                            main() + ": removing a point",
                            "Release to remove this point",
                        ),
                    },
                }
            }

            Focus::Selection { .. } => {
                let main = || {
                    let len = self.selection.len();
                    Cow::from(format!("{len} block{}", if len == 1 { "" } else { "s" }))
                };
                match *cursor {
                    Buttons {
                        left: false,
                        meta: false,
                        ..
                    } => (
                        main(),
                        match ctx.special_action() {
                            SpecialAction::Select => "Shift - zoom, Meta - select points",
                            SpecialAction::Add => "Shift - zoom, Meta - add a point",
                            SpecialAction::Remove => "Shift - zoom, Meta - remove points",
                        },
                    ),

                    Buttons {
                        left: false,
                        meta: true,
                        ..
                    } => match ctx.special_action() {
                        // TODO: find a way to replace `format!` with smth const
                        SpecialAction::Select => (
                            Cow::from(T::EDITOR_NAME) + ": selecting",
                            "Press and hold left mouse button to select",
                        ),
                        SpecialAction::Add => (
                            Cow::from(T::EDITOR_NAME) + ": adding a point",
                            "Click on empty space to add a point",
                        ),
                        SpecialAction::Remove => (
                            main() + ": removing selection",
                            "Click to remove this point",
                        ),
                    },

                    Buttons {
                        left: true,
                        meta: false,
                        ..
                    } => (main() + ": moving", "Release to stop"),

                    Buttons {
                        left: true,
                        meta: true,
                        ..
                    } => match ctx.special_action() {
                        SpecialAction::Select => (
                            Cow::from(T::EDITOR_NAME) + ": selecting",
                            "Release to select",
                        ),
                        SpecialAction::Add => (
                            Cow::from(T::EDITOR_NAME) + ": adding a point",
                            "Release and click on empty space to add a point",
                        ),
                        SpecialAction::Remove => (
                            main() + ": removing selection",
                            "Release to remove the selection",
                        ),
                    },
                }
            }
        };
        ctx.emit_event(AppEvent::SetHint(main, aux.into()));
    }

    fn set_zoom_focus(&mut self, cursor: Cursor) {
        self.focus = Focus::Zoom {
            pivot: cursor.point + self.offset,
            init_offset: self.offset,
            init_scale: self.scale,
        };
        self.update_hint = true;
    }

    fn set_plane_focus(&mut self) {
        self.focus = Focus::Plane {
            origin: default(),
            init_offset: default(),
        };
        self.update_hint = true;
    }

    fn set_point_focus(&mut self, id: usize) {
        self.focus = Focus::Point {
            id,
            last_loc: default(),
            origin: default(),
            meta: false,
        };
        self.update_hint = true;
    }

    fn set_selection_focus(&mut self) {
        self.focus = Focus::Selection {
            origin: default(),
            end: default(),
            meta: false,
        };
        self.update_hint = true;
    }

    fn special_action_on_drag(
        &mut self,
        _ctx: ContextMut,
        _sequencer: &Sequencer,
        _cur_loc: [R64; 2],
    ) -> AppResult<()> {
        Ok(())
    }

    /// Executes the selected special action after a click.
    fn special_action_on_click(
        &mut self,
        mut ctx: ContextMut,
        sequencer: &Sequencer,
        pressed_at: [R64; 2],
        released_at: [R64; 2],
        visual_ctx: impl Deref<Target = T::VisualContext>,
    ) -> AppResult<()> {
        Ok(match ctx.special_action() {
            SpecialAction::Select => {
                let area = [pressed_at, released_at]
                    .flipped()
                    .map(|x| (x[0]..=x[1]).ordered());
                let prev_ids = self.inner.selection.to_box();
                self.inner.selection = self
                    .data
                    .iter()
                    .enumerate()
                    .filter_map(|(id, x)| {
                        x.in_hitbox(&area, ctx.as_ref(), sequencer, *visual_ctx)
                            .report()
                            .unwrap_or(false)
                            .then_some(id)
                    })
                    .collect();
                let prev_src = replace(
                    &mut self.selection_src,
                    area.clone().map(|x| x.into_inner().0),
                );
                let prev_size = replace(
                    &mut self.selection_size,
                    area.clone()
                        .map(|x| x.into_inner())
                        .map(|(start, end)| end - start),
                );
                ctx.register_action(EditorAction::SetSelection {
                    editor_id: self.inner.id,
                    prev_ids,
                    prev_src,
                    prev_size,
                    cur_ids: self.selection.to_box(),
                    cur_src: self.selection_src,
                    cur_size: self.selection_size,
                });
                T::on_selection_change(self, ctx)?;
            }

            SpecialAction::Add => {
                if !matches!(self.focus, Focus::Point { .. }) {
                    let new = T::create(self, released_at);
                    let point_id = self.data.len();
                    self.data.push(new);
                    ctx.register_action(EditorAction::AddPoint {
                        editor_id: self.inner.id,
                        point_id,
                        point_loc: released_at,
                    })
                }
            }

            SpecialAction::Remove => match self.focus {
                Focus::Point { id, .. } => ctx.register_action(self.remove_points(once(id))?),
                Focus::Selection { .. } => ctx.register_action(self.filter_selected(|_| true)),
                _ => (),
            },
        })
    }

    fn handle_hover(
        &mut self,
        cursor: Option<Cursor>,
        mut ctx: ContextMut,
        sequencer: &Sequencer,
        // lazy evaluation :D
        visual_ctx: impl Deref<Target = T::VisualContext>,
    ) -> AppResult<()> {
        let Some(cursor) = cursor else {
            self.focus = Focus::None;
            return Ok(());
        };

        let size = self
            .canvas
            .cast::<HtmlCanvasElement>()
            .to_app_result()?
            .size();
        let snap_step = [ctx.snap_step(), T::Y_SNAP];
        let step = &R64::array_from(size).div(&self.scale);

        let cursor_point_user = LazyCell::new({
            let off = self.offset;
            move || R64::array_from(cursor.point + off).div(step)
        });
        let cursor_point_user_aligned_confined =
            LazyCell::new(|| [T::X_BOUND, T::Y_BOUND].fit(cursor_point_user.floor_to(snap_step)));

        match &mut self.inner.focus {
            Focus::None => self.set_plane_focus(),

            Focus::Zoom {
                init_offset,
                init_scale,
                pivot,
            } => {
                if cursor.left {
                    if !self.inner.last_cursor.left {
                        *pivot = cursor.point + self.inner.offset;
                        *init_scale = self.inner.scale;
                        *init_offset = self.inner.offset;
                    } else {
                        self.inner.redraw = true;
                    }
                    let point = cursor.point + *init_offset - *pivot;
                    if !T::SCALE_X_BOUND.is_empty() {
                        self.inner.scale[0] =
                            T::SCALE_X_BOUND.fit(r64![50] / size[1] * point.x + init_scale[0]);
                        self.inner.offset.x = ((init_scale[0] - self.inner.scale[0]) * pivot.x
                            / self.inner.scale[0]
                            + init_offset.x)
                            .into();
                    }
                    if !T::SCALE_Y_BOUND.is_empty() {
                        self.inner.scale[1] =
                            T::SCALE_Y_BOUND.fit(r64![50] / size[0] * point.y + init_scale[1]);
                        self.inner.offset.y = ((init_scale[1] - self.inner.scale[1]) * pivot.y
                            / self.inner.scale[1]
                            + init_offset.y)
                            .into();
                    }
                } else {
                    if self.inner.last_cursor.left {
                        let offset_delta = self.inner.offset - *init_offset;
                        let scale_delta = self.inner.scale.sub(init_scale);
                        if !offset_delta.is_zero() || scale_delta.any(|x| **x != 0.0) {
                            ctx.register_action(EditorAction::DragPlane {
                                editor_id: self.inner.id,
                                offset_delta,
                                scale_delta,
                            });
                        }
                    }
                    if cursor.shift {
                        *pivot = cursor.point;
                        self.redraw = true;
                    } else {
                        self.set_plane_focus()
                    }
                }
            }

            Focus::Plane {
                origin,
                init_offset,
            } => match *cursor {
                Buttons {
                    shift: true,
                    left: false,
                    ..
                } => self.set_zoom_focus(cursor),

                Buttons {
                    left: false, meta, ..
                } => {
                    if self.inner.last_cursor.left {
                        let cur_loc = *cursor_point_user_aligned_confined;
                        if self.inner.last_cursor.meta {
                            let origin = *origin;
                            self.special_action_on_click(
                                ctx.as_mut(),
                                sequencer,
                                origin,
                                cur_loc,
                                visual_ctx,
                            )?;
                        } else {
                            let init_offset = *init_offset;
                            ctx.register_action(EditorAction::DragPlane {
                                editor_id: self.inner.id,
                                offset_delta: self.inner.offset - init_offset,
                                scale_delta: default(),
                            });
                        }
                    } else if self.point_in_selection(*cursor_point_user) {
                        self.set_selection_focus()
                    } else if let Some(p) =
                        self.point_by_pos(*cursor_point_user, ctx.as_mut(), sequencer, *visual_ctx)?
                    {
                        self.set_point_focus(p.index())
                    } else {
                        self.redraw |= self.last_cursor.meta | meta;
                    }
                }

                Buttons {
                    left: true, meta, ..
                } => {
                    if meta {
                        if !self.inner.last_cursor.left {
                            *origin = *cursor_point_user_aligned_confined;
                        } else {
                            self.inner.redraw = true
                        }
                        self.special_action_on_drag(
                            ctx.as_mut(),
                            sequencer,
                            *cursor_point_user_aligned_confined,
                        )?;
                    } else {
                        if !self.inner.last_cursor.left {
                            *init_offset = self.inner.offset;
                        } else {
                            self.inner.redraw = true
                        }

                        if !T::OFFSET_X_BOUND.is_empty() {
                            self.inner.offset.x = T::OFFSET_X_BOUND
                                .map_bounds(|x| x * step[0])
                                .extend(self.inner.offset.x)
                                .fit(
                                    self.inner.offset.x + self.inner.last_cursor.point.x
                                        - cursor.point.x,
                                );
                        }
                        if !T::OFFSET_Y_BOUND.is_empty() {
                            self.inner.offset.y = T::OFFSET_Y_BOUND
                                .map_bounds(|y| y * step[1])
                                .extend(self.inner.offset.y)
                                .fit(
                                    self.inner.offset.y + self.inner.last_cursor.point.y
                                        - cursor.point.y,
                                );
                        }
                    }
                }
            },

            Focus::Point {
                id,
                last_loc,
                origin,
                meta,
            } => {
                if cursor.left {
                    if *meta {
                        self.special_action_on_drag(
                            ctx.as_mut(),
                            sequencer,
                            *cursor_point_user_aligned_confined,
                        )?;
                    } else {
                        let delta = if !self.inner.last_cursor.left {
                            *last_loc = *cursor_point_user_aligned_confined;
                            *origin = *last_loc;
                            *meta = cursor.meta;
                            default()
                        } else {
                            let new = *cursor_point_user_aligned_confined;
                            new.sub(&replace(last_loc, new))
                        };
                        if delta.any(|x| *x != 0) {
                            let id = *id;
                            T::move_point(Ok(self.data.get_mut(id).to_app_result()?), delta, false);
                            self.redraw = true;
                            T::on_move(self, ctx.as_mut(), cursor, delta, Some(id))?
                        }
                    }
                } else if self.inner.last_cursor.left {
                    let (dst, src, point_id) = (*cursor_point_user_aligned_confined, *origin, *id);
                    if *meta {
                        self.special_action_on_click(
                            ctx.as_mut(),
                            sequencer,
                            src,
                            dst,
                            visual_ctx,
                        )?;
                    } else {
                        let delta = dst.sub(&src);
                        if delta.any(|x| *x != 0) {
                            ctx.register_action(EditorAction::DragPoint {
                                editor_id: self.id,
                                point_id,
                                delta,
                            });
                        }
                    }
                } else if cursor.shift {
                    self.set_zoom_focus(cursor)
                } else if !self.data.get(*id).to_app_result()?.in_hitbox(
                    &cursor_point_user.map(|x| x..=x),
                    ctx.as_ref(),
                    sequencer,
                    *visual_ctx,
                )? {
                    self.set_plane_focus()
                }
            }

            Focus::Selection { origin, end, meta } => {
                if cursor.left {
                    let delta = if !self.inner.last_cursor.left {
                        *end = *cursor_point_user_aligned_confined;
                        *origin = *end;
                        *meta = cursor.meta;
                        default()
                    } else {
                        let new = *cursor_point_user_aligned_confined;
                        new.sub(&replace(end, new))
                    };
                    if delta.any(|x| *x != 0) {
                        self.inner.redraw = true;
                        for &id in self.inner.selection.iter() {
                            T::move_point(
                                Ok(unsafe { self.data.get_unchecked_mut(id) }),
                                delta,
                                *meta,
                            );
                        }
                        T::move_point(Err(&mut self.inner.selection_src), delta, *meta);
                        T::on_move(self, ctx.as_mut(), cursor, delta, None)?
                    }
                } else if self.inner.last_cursor.left {
                    let (meta, src, dst) =
                        (*meta, take(origin), *cursor_point_user_aligned_confined);
                    *end = default();
                    if meta {
                        self.special_action_on_click(
                            ctx.as_mut(),
                            sequencer,
                            src,
                            dst,
                            visual_ctx,
                        )?;
                    } else {
                        ctx.register_action(EditorAction::DragSelection {
                            editor_id: self.id,
                            delta: dst.sub(&src),
                        });
                    }
                } else if cursor.shift {
                    self.set_zoom_focus(cursor)
                } else if !self.point_in_selection(*cursor_point_user) {
                    if let Some(p) =
                        self.point_by_pos(*cursor_point_user, ctx.as_mut(), sequencer, *visual_ctx)?
                    {
                        self.set_point_focus(p.index())
                    } else {
                        self.set_plane_focus()
                    }
                }
            }
        };

        let old_buttons = *replace(&mut self.last_cursor, cursor);
        let buttons_changed = old_buttons != *cursor;
        self.redraw |= buttons_changed;
        Ok(if self.update_hint | buttons_changed {
            self.update_hint = false;
            self.update_hint(ctx, cursor);
        })
    }

    /// an offset of 0 is assumed
    /// the returned array are the actual bounds of the rendered grid in user coordinates
    fn draw_grid(
        canvas_size: [R64; 2],
        step: [R64; 2],
        scale: [R64; 2],
    ) -> AppResult<(Path2d, [R64; 2])> {
        let res = Path2d::new()?;
        let steps: [usize; 2] = [T::X_BOUND.end, T::Y_BOUND.end]
            .mul(&step)
            .zip(canvas_size, min)
            .div(&canvas_size)
            .mul(&scale)
            .into_array();

        for x in 0..steps[0] {
            for y in (0..steps[1]).step_by(2) {
                let [x, y] = [step[0] * x, step[1] * y];
                res.rect(
                    *x + AnyGraphEditor::LINE_WIDTH,
                    *y,
                    *step[0] - AnyGraphEditor::LINE_WIDTH,
                    *step[1],
                );
                res.rect(*x, *y + *step[1], AnyGraphEditor::LINE_WIDTH, *step[1]);
            }
        }

        Ok((res, scale))
    }

    pub fn handle_event(
        &mut self,
        event: &AppEvent,
        mut ctx: ContextMut,
        sequencer: &Sequencer,
        visual_ctx: impl FnOnce() -> T::VisualContext,
    ) -> AppResult<()> {
        Ok(match event {
            AppEvent::Enter(id, e) | AppEvent::Hover(id, e) if *id == self.id => self
                .handle_hover(
                    Some(e.try_into()?),
                    ctx,
                    sequencer,
                    LazyCell::new(visual_ctx),
                )?,

            AppEvent::Focus(id, e) if *id == self.id => {
                e.target_dyn_into::<Element>()
                    .to_app_result()?
                    .set_pointer_capture(e.pointer_id())?;
                self.handle_hover(
                    Some(e.try_into()?),
                    ctx,
                    sequencer,
                    LazyCell::new(visual_ctx),
                )?;
            }

            AppEvent::SetSpecialAction(_) => {
                if !matches!(self.focus, Focus::None) {
                    self.update_hint(ctx.as_mut(), self.last_cursor);
                    self.handle_hover(
                        Some(self.last_cursor),
                        ctx.as_mut(),
                        sequencer,
                        LazyCell::new(visual_ctx),
                    )?;
                }
            }

            AppEvent::KeyRelease(id, e) if *id == self.id => self.handle_hover(
                Some(self.last_cursor + e),
                ctx,
                sequencer,
                LazyCell::new(visual_ctx),
            )?,

            AppEvent::Leave(id) if *id == self.id => {
                self.handle_hover(None, ctx, sequencer, LazyCell::new(visual_ctx))?
            }

            AppEvent::Resize => self.init()?,

            AppEvent::StartPlay(_) => self.redraw = true,

            AppEvent::Undo(actions) => {
                for action in actions.iter() {
                    match *action {
                        EditorAction::DragPlane {
                            editor_id,
                            offset_delta,
                            scale_delta,
                        } => {
                            if editor_id == self.id {
                                self.redraw = true;
                                self.offset -= offset_delta;
                                self.scale = self.scale.sub(&scale_delta);
                            }
                        }

                        EditorAction::DragPoint {
                            editor_id,
                            point_id,
                            mut delta,
                        } => {
                            if editor_id == self.id {
                                self.redraw = true;
                                delta = delta.map(|x| -x);
                                T::move_point(
                                    Ok(self.data.get_mut(point_id).to_app_result()?),
                                    delta,
                                    false,
                                )
                            }
                        }

                        EditorAction::DragSelection {
                            editor_id,
                            mut delta,
                        } => {
                            if editor_id == self.id {
                                self.redraw = true;
                                delta = delta.map(|x| -x);
                                for &id in self.inner.selection.iter() {
                                    T::move_point(
                                        Ok(self.data.get_mut(id).to_app_result()?),
                                        delta,
                                        false,
                                    );
                                }
                                T::move_point(Err(&mut self.selection_src), delta, false)
                            }
                        }

                        EditorAction::SetSelection {
                            editor_id,
                            ref prev_ids,
                            prev_src,
                            prev_size,
                            ..
                        } => {
                            if editor_id == self.id {
                                self.redraw = true;
                                self.selection = prev_ids.to_vec();
                                self.selection_src = prev_src;
                                self.selection_size = prev_size;
                            }
                        }

                        EditorAction::AddPoint {
                            editor_id,
                            point_id,
                            ..
                        } if editor_id == self.id => _ = self.remove_points(once(point_id))?,

                        EditorAction::RemovePoint(editor_id, ref points)
                            if editor_id == self.id =>
                        {
                            for &RemovedPoint {
                                ref point,
                                index,
                                was_selected,
                            } in points.iter()
                            {
                                self.data.insert(
                                    index,
                                    point.downcast_ref::<T>().to_app_result()?.clone(),
                                );
                                if was_selected {
                                    self.selection.push(index);
                                }
                            }
                        }

                        _ => (),
                    }
                }
            }

            AppEvent::Redo(actions) => {
                for action in actions.iter() {
                    match *action {
                        EditorAction::DragPlane {
                            editor_id,
                            offset_delta,
                            scale_delta,
                        } => {
                            if editor_id == self.id {
                                self.redraw = true;
                                self.offset += offset_delta;
                                self.scale = self.scale.add(&scale_delta);
                            }
                        }

                        EditorAction::DragPoint {
                            editor_id,
                            point_id,
                            delta,
                        } => {
                            if editor_id == self.id {
                                self.redraw = true;
                                T::move_point(
                                    Ok(self.data.get_mut(point_id).to_app_result()?),
                                    delta,
                                    false,
                                )
                            }
                        }

                        EditorAction::DragSelection { editor_id, delta } => {
                            if editor_id == self.id {
                                self.redraw = true;
                                for &id in self.inner.selection.iter() {
                                    T::move_point(
                                        Ok(self.data.get_mut(id).to_app_result()?),
                                        delta,
                                        false,
                                    );
                                }
                                T::move_point(Err(&mut self.selection_src), delta, false)
                            }
                        }

                        EditorAction::SetSelection {
                            editor_id,
                            ref cur_ids,
                            cur_src,
                            cur_size,
                            ..
                        } => {
                            if editor_id == self.id {
                                self.redraw = true;
                                self.selection = cur_ids.to_vec();
                                self.selection_src = cur_src;
                                self.selection_size = cur_size;
                            }
                        }

                        EditorAction::AddPoint {
                            editor_id,
                            point_id,
                            point_loc,
                        } => {
                            if editor_id == self.id {
                                let new = T::create(self, point_loc);
                                self.data.insert(point_id, new);
                            }
                        }

                        EditorAction::RemovePoint(editor_id, ref points)
                            if editor_id == self.id =>
                        {
                            _ = self.remove_points(points.iter().map(|x| x.index))?
                        }

                        _ => (),
                    }
                }
            }

            AppEvent::Frame(_) if self.redraw => {
                let canvas: HtmlCanvasElement = self.canvas.cast().to_app_result()?;
                let size = canvas.size().map(R64::from);
                let snap_step = [ctx.snap_step(), T::Y_SNAP];
                let canvas_ctx = canvas.get_2d_context()?;

                let step = &size.div(&self.scale);
                let offset_x = R64::from(-self.offset.x)
                    % (self.offset.x > (T::X_BOUND.start * step[0]).into())
                        .choose(step[0], R64::INFINITY);
                let offset_y = R64::from(-self.offset.y)
                    % (self.offset.y > (T::Y_BOUND.start * step[1]).into())
                        .choose(step[1] * 2.0, R64::INFINITY);

                let offset = &R64::array_from(self.offset);
                let to_user = |loc| R64::array_from(loc).add(offset).div(step);
                let to_aligned_canvas = |loc: Point| loc.floor_to(snap_step.mul(step).into());
                let confine = |x| [T::X_BOUND, T::Y_BOUND].fit(x);

                canvas_ctx.set_fill_style(&AnyGraphEditor::BG_STYLE.into());
                canvas_ctx.fill_rect(0.0, 0.0, *size[0], *size[1]);

                let (grid, original_scale) = self
                    .inner
                    .grid
                    .get_or_try_insert(|| Self::draw_grid(size, *step, self.inner.scale))?;
                let grid_scale = original_scale.div(&self.inner.scale);
                let reps = self
                    .inner
                    .scale
                    .sub(&[offset_x, offset_y].div(step))
                    .div(original_scale)
                    .map(|x| usize::from(x.ceil()));

                canvas_ctx.set_fill_style(&AnyGraphEditor::MG_STYLE.into());
                canvas_ctx.transform(
                    *grid_scale[0],
                    0.0,
                    0.0,
                    *grid_scale[1],
                    *offset_x,
                    *offset_y,
                )?;

                for _ in 0..reps[1] {
                    for _ in 0..reps[0] {
                        canvas_ctx.fill_with_path_2d(grid);
                        canvas_ctx.translate(*size[0], 0.0)?;
                    }
                    canvas_ctx.translate(-*size[0] * reps[0] as f64, *size[1])?;
                }
                canvas_ctx.reset_transform()?;

                let solid = Path2d::new()?;
                let dotted = Path2d::new()?;
                let [x, y] = self.selection_src.mul(step).sub(offset);
                let [w, h] = self.selection_size.mul(step);
                dotted.rect(*x, *y, *w, *h);

                match self.focus {
                    Focus::Zoom {
                        pivot, init_offset, ..
                    } => {
                        if self.last_cursor.left {
                            let [x, y] = (pivot - init_offset).map(|x| x as f64);
                            solid.move_to(x - 10.0, y);
                            solid.line_to(x + 10.0, y);
                            solid.move_to(x, y - 10.0);
                            solid.line_to(x, y + 10.0);
                        } else {
                            canvas_ctx.set_text_align("left");
                            canvas_ctx.set_text_baseline("bottom");
                            canvas_ctx.set_fill_style(&AnyGraphEditor::FG_STYLE.into());
                            canvas_ctx.fill_text(
                                &T::fmt_loc(confine(to_user(pivot))),
                                5.0,
                                *size[1] - 5.0,
                            )?;
                        }
                    }

                    Focus::Plane { origin, .. } if self.last_cursor.meta => {
                        if self.last_cursor.left {
                            let cur = to_aligned_canvas(self.last_cursor.point);
                            let origin = origin.mul(step).sub(offset).map(|x| *x);
                            dotted.rect(
                                origin[0],
                                origin[1],
                                cur.x as f64 - origin[0],
                                cur.y as f64 - origin[1],
                            );
                        } else {
                            let [x, y] = self.last_cursor.point.map(|x| x as f64);
                            dotted.move_to(-*size[0], y);
                            dotted.line_to(*size[0], y);
                            dotted.move_to(x, -*size[1]);
                            dotted.line_to(x, *size[1]);
                        }
                    }

                    _ => (),
                }

                T::on_redraw(
                    self,
                    ctx.as_ref(),
                    sequencer,
                    &size,
                    &solid,
                    &dotted,
                    visual_ctx(),
                )?;

                canvas_ctx.set_stroke_style(&AnyGraphEditor::FG_STYLE.into());
                canvas_ctx.fill_with_path_2d(&solid);
                canvas_ctx.stroke_with_path(&solid);
                canvas_ctx
                    .set_line_dash(eval_once!(JsValue: js_array![number 10.0, number 10.0]))?;
                canvas_ctx.stroke_with_path(&dotted);
                canvas_ctx.set_line_dash(eval_once!(JsValue: js_array![]))?;
            }

            _ => (),
        })
    }
}
