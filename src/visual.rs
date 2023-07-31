use std::{
    iter::Iterator,
    slice::{from_raw_parts, from_mut},
    mem::{replace, take},
    ops::{Range, Deref, Not, DerefMut},
    fmt::Debug,
    rc::Rc,
    cmp::{min, Ordering},
    cell::{LazyCell, Cell}};
use web_sys::{
    HtmlCanvasElement,
    AnalyserNode,
    ImageData,
    HtmlElement,
    Path2d,
    AudioContext, 
    SvgElement, 
    Element};
use wasm_bindgen::{Clamped, JsValue, JsCast};
use yew::{TargetCast, NodeRef};
use crate::{
    utils::{SliceExt, Point,
        JsResult, HtmlCanvasExt, JsResultUtils, OptionExt,
        HtmlElementExt, 
        Pipe, BoolExt, RangeExt, VecExt, R64, ArrayExt,
        ArrayFrom, IntoArray, SliceRef, ResultToJsResult,
        SliceMove, RoundTo, FlippedArray, default, WasmCell,
        Alias, ToEveryNth, ToIterIndicesMut},
    sound::{FromBeats, Secs},
    global::{AppEvent, AppContext, AppAction},
    input::{Buttons, Cursor},
    loc,
    r64,
    js_assert,
    eval_once,
    js_array
};

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Rgba {
	pub r: u8,
	pub g: u8,
	pub b: u8,
	pub a: u8
}

impl From<u32> for Rgba {
	fn from(val: u32) -> Self {
		Self {
			r: (val >> 24) as u8,
			g: ((val >> 16) & 0xFF) as u8,
			b: ((val >>  8) & 0xFF) as u8,
			a: (val & 0xFF) as u8}
	}
}

impl From<Rgba> for u32 {
	fn from(val: Rgba) -> Self {
		val.a as u32
		| (val.b as u32) << 8
		| (val.g as u32) << 16
		| (val.r as u32) << 24
	}
}

impl std::fmt::Display for Rgba {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "#{:02X}{:02X}{:02X}{:02X}", self.r, self.g, self.b, self.a)
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
		a: (lower.a as f32 * weight_recip + upper.a as f32 * weight) as u8}
}

pub struct SoundVisualiser {
    input: Rc<AnalyserNode>,
	out_data: Vec<Rgba>,
	in_data: Vec<u8>,
    gradient: Vec<Rgba>,
    canvas: NodeRef,
    width: u32, height: u32
}

impl SoundVisualiser {
	pub const FG: Rgba = Rgba{r:0x00, g:0x69, b:0xE1, a:0xFF};
	pub const BG: Rgba = Rgba{r:0x18, g:0x18, b:0x18, a:0xFF};
	pub fn new(ctx: &AudioContext) -> JsResult<Self> {
		Ok(Self{input: Rc::new(ctx.create_analyser().add_loc(loc!())?),
            out_data: vec![], in_data: vec![],
            gradient: (0 ..= u8::MAX)
                .map(|i| interp(&[Self::BG, Self::FG], i))
                .collect(),
			width: 0, height: 0, canvas: NodeRef::default()})
	}

    #[inline] pub fn canvas(&self) -> &NodeRef {&self.canvas}

    #[inline] pub fn input(&self) -> &Rc<AnalyserNode> {&self.input}

    // TODO: correctly readjust the graph when shrinked in the UI
    pub fn handle_event(&mut self, event: &AppEvent, ctx: &AppContext) -> JsResult<()> {
        Ok(match event {
            AppEvent::Resize => {
                let canvas: HtmlCanvasElement = self.canvas.cast().to_js_result(loc!())?;
                let [w, h] = canvas.client_size().map(|x| x as u32);
                canvas.set_width(w);
                canvas.set_height(h);
                self.width = w;
                self.height = h;
                self.in_data.resize(w as usize, 0);
                self.out_data.resize(w as usize * w as usize, Self::BG);
            }

            AppEvent::Frame(..) if ctx.play_since().is_finite() => {
                self.out_data.rotate_right(1);
                self.input.get_byte_frequency_data(&mut self.in_data);
                for (&src, dst) in self.in_data.iter().zip(self.out_data.every_nth_mut(self.width as usize)) {
                    *dst = unsafe {*self.gradient.get_unchecked(src as usize)};
                }

                let out = unsafe{from_raw_parts(self.out_data.as_ptr().cast(), self.out_data.len() * 4)};
                let out = ImageData::new_with_u8_clamped_array(Clamped(out), self.width).add_loc(loc!())?;
                self.canvas.cast::<HtmlCanvasElement>().to_js_result(loc!())?
                    .get_2d_context(loc!())?
                    .put_image_data(&out, 0.0, 0.0).add_loc(loc!())?;
            }

            _ => (),
        })
    }
}

#[derive(Debug, PartialEq, Default)]
pub struct HintHandler {
    main_bar: NodeRef,
    aux_bar: NodeRef
}

impl HintHandler {
    pub fn handle_event(&mut self, event: &AppEvent, _: &AppContext) -> JsResult<()> {
        Ok(match event {
            AppEvent::SetHint(main, aux) => {
                self.main_bar.cast::<HtmlElement>().to_js_result(loc!())?
                    .set_inner_text(main);
                self.aux_bar.cast::<HtmlElement>().to_js_result(loc!())?
                    .set_inner_text(aux);
            }

            AppEvent::FetchHint(e) => {
                let main_bar: HtmlElement = self.main_bar.cast().to_js_result(loc!())?;
                let  aux_bar: HtmlElement = self.aux_bar.cast().to_js_result(loc!())?;
                let mut src: Element = e.target_dyn_into().to_js_result(loc!())?;

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
                        break default()
                    }
                };
            }

            _ => ()
        })
    }

    #[inline] pub fn main_bar(&self) -> &NodeRef {&self.main_bar}

    #[inline] pub fn aux_bar(&self) -> &NodeRef {&self.aux_bar}
}

/// data that can be edited with a generic graph editor defined below
pub trait GraphPoint: Sized + Ord {
    /// the name of the plane that will be displayed as a hint when hovered over it
    const EDITOR_NAME: &'static str;
    /// bounds for the points along the X axis
    const X_BOUND: Range<R64> = r64![0.0] .. R64::INFINITY;
    /// bounds for the scale of the X axis of the graph
    const SCALE_X_BOUND: Range<R64> = r64![3.0] .. r64![30.0];
    /// bounds for the offset of the X axis of the graph
    const OFFSET_X_BOUND: Range<R64> = r64![-1.0] .. R64::INFINITY;
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
    /// returns `true` if the given user coordinates are inside the hitbox of the point
    fn in_hitbox(&self, point: [R64; 2]) -> bool;

    ////// HANDLERS
    /// Handle points being moved in the UI.
    #[allow(unused_variables)] #[inline] fn on_move(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        cursor: Cursor,
        delta: [R64; 2]
    ) {} 
    /// Handle the editor space being clicked, i.e. pressed & released.
    /// The current selection will be available in the `editor` itself.
    /// If `old_selection` is `None`, it means that it didn't change.
    /// If `old_selection` is `Some`, it doesn't mean that `editor.selection != old_selection`,
    /// it just means that selection was changed.
    /// Returns the action to register after the click; if `None` is returned, the default action
    /// will be chosen by the graph editor itself.
    #[allow(unused_variables)] #[inline] fn on_click(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        cursor: Cursor,
        pressed_at:    impl Deref<Target = [R64; 2]>,
        released_at:   impl Deref<Target = [R64; 2]>,
        old_selection: Option<&[usize]>
    ) -> Option<AppAction> {None}
    /// Handle cursor moving across the editor plane (not across a selection or a point).
    /// `loc` is in user coordinates; the location in canvas coordinates is in `cursor`.
    /// `first` signifies whether this call is the first consecutive one.
    /// Note: if shift's held down, hovering over a point/selection won't count as such, and this
    /// Handler will still be called instead of `on_(point/selection)_hover`.
    #[allow(unused_variables)] #[inline] fn on_plane_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        cursor: Cursor,
        loc: impl Deref<Target = [R64; 2]>,
        first: bool
    ) {}
    /// Handle cursor moving across a point on the editor plane.
    /// `point_id` is the index of the point in the editor.
    /// `first` signifies whether this call is the first consecutive one.
    #[allow(unused_variables)] #[inline] fn on_point_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        cursor: Cursor,
        point_id: usize,
        first: bool
    ) {}
    /// Handle cursor moving across a selection on the editor plane.
    /// The selection itself is available in `editor`.
    /// `first` signifies whether this call is the first consecutive one.
    #[allow(unused_variables)] #[inline] fn on_selection_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        cursor: Cursor,
        first: bool
    ) {}
    /// Handle the user canceling an action.
    /// The handler is only called if the graph editor doesn't know how to handle the action.
    #[allow(unused_variables)] #[inline] fn on_undo(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        action: &AppAction
    ) {}
    /// Handle the user canceling cancellation of an action.
    /// The handler is only called if the graph editor doesn't know how to handle the action.
    #[allow(unused_variables)] #[inline] fn on_redo(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        action: &AppAction
    ) {}

    /// `loc` is in user coordinates
    fn fmt_loc(loc: [R64; 2]) -> String;

    /// visual representation of the point's hitbox
    /// `mapper` maps user coordinates to canvas coordinates, all vertices in the
    /// returned `Path2d` must be the result of the `mapper` function
    fn draw(&self, next: Option<&Self>, mapper: impl Fn([R64; 2]) -> [R64; 2]) -> JsResult<Path2d>;
    /// the canvas's coordinate space
    /// the function will be called every time the user 
    #[inline] fn canvas_coords(canvas: &HtmlCanvasElement) -> JsResult<[u32; 2]> {
        Ok([canvas.client_width() as u32, canvas.client_height() as u32])
    }
}

/// a special reference wrapper: access to everything is immutable,
/// except for the inner value, which is mutable
pub struct GraphPointView<'a, T: GraphPoint>(&'a mut T);

impl<'a, T: GraphPoint> Deref for GraphPointView<'a, T> {
    type Target = T;
    #[inline] fn deref(&self) -> &Self::Target {self.0}
}

impl<'a, T: GraphPoint> GraphPointView<'a, T> {
    #[inline] pub fn inner(&mut self) -> &mut T::Inner {self.0.inner_mut()}
    #[inline] pub fn y(&mut self) -> &mut T::Y {self.0.y_mut()}

    // /// the caller must ensure that the point retains its sorted placement
    // #[inline] pub unsafe fn unlock(self) -> &'a mut T {self.0}
}

// types as coordinate space hints:
/// in canvas coordinates with plane offset
type OffsetCanvasPoint = Point;
/// in user coordinates, aligned to snap step and limited to bounds specified by `GraphPoint::(X/Y)_BOUND`
type ConfinedAlignedUserPoint = [R64; 2];

#[derive(Default, Debug, Clone, PartialEq, Eq)]
enum Focus {
    #[default] None,
    Zoom{init_offset: Point, pivot: OffsetCanvasPoint, init_scale: [R64; 2]},
    Plane{origin: ConfinedAlignedUserPoint, init_offset: Point, press_time: Secs},
    Point{id: usize, last_loc: ConfinedAlignedUserPoint, origin: ConfinedAlignedUserPoint, meta: bool},
    Selection{origin: ConfinedAlignedUserPoint, end: ConfinedAlignedUserPoint, meta: bool}
}

static GRAPH_EDITOR_COUNT: WasmCell<Cell<usize>> = WasmCell::new(Cell::new(AnyGraphEditor::INVALID_ID + 1));

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
    changed_focus: bool,
    grid: Option<(Path2d, [R64; 2])>,
    id: usize
}

impl AnyGraphEditor {
    const FONT: &str = "20px consolas";
    const BG_STYLE: &str = "#232328";
    const MG_STYLE: &str = "#333338";
    const FG_STYLE: &str = "#0069E1";
    const LINE_WIDTH: f64 = 3.0;
    /// an ID that's guaranteed to never be used by any graph editor
    pub const INVALID_ID: usize = 0;

    #[inline] pub fn id(&self) -> usize {self.id}

    #[inline] pub fn last_cursor(&self) -> Option<Cursor> {
        matches!(self.focus, Focus::None).not().then_some(self.last_cursor)
    }

    #[inline] pub fn canvas(&self) -> &NodeRef {&self.canvas}

    #[inline] pub fn selection(&self) -> &[usize] {&self.selection}

}

#[derive(Debug, Clone, PartialEq)]
pub struct GraphEditor<T: GraphPoint> {
    inner: AnyGraphEditor,
    data: Vec<T>
}

impl<T: GraphPoint> Deref for GraphEditor<T> {
    type Target = AnyGraphEditor;
    #[inline] fn deref(&self) -> &Self::Target {&self.inner}
}

impl<T: GraphPoint> DerefMut for GraphEditor<T> {
    #[inline] fn deref_mut(&mut self) -> &mut Self::Target {&mut self.inner}
}

impl<T: GraphPoint> Default for GraphEditor<T> {
    #[inline] fn default() -> Self {Self::new(vec![])}
}

impl<T: GraphPoint> GraphEditor<T> {
    #[inline] pub fn new(data: Vec<T>) -> Self {
        let res = Self{data,
            inner: AnyGraphEditor{
                scale: [T::SCALE_X_BOUND, T::SCALE_Y_BOUND]
                    .map(|x| x.to_pair().mul(&[0.75, 0.25]).sum()),
                id: GRAPH_EDITOR_COUNT.get(),
                ..default()}};
        GRAPH_EDITOR_COUNT.set(res.id + 1);
        res
    }

    #[inline] pub fn len(&self) -> usize {self.data.len()}

    #[inline] pub fn get(&self, index: usize) -> Option<&T> {
        self.data.get(index)
    }

    #[inline] pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        self.data.get_unchecked(index)
    }

    #[inline] pub unsafe fn get_unchecked_aware(&self, index: usize) -> SliceRef<'_, T> {
        SliceRef::raw(self.get_unchecked(index), index)
    }

    #[inline] pub unsafe fn get_unchecked_mut(&mut self, index: usize)
    -> GraphPointView<'_, T> {
        GraphPointView(self.data.get_unchecked_mut(index))
    }

    #[inline] pub unsafe fn first_unchecked(&self) -> &T {
        &*self.data.as_ptr()
    }

    #[inline] pub unsafe fn last_unchecked(&self) -> &T {
        unsafe{self.data.last().unwrap_unchecked()}
    }

    #[inline] pub fn iter_mut(&mut self) -> impl Iterator<Item=GraphPointView<'_, T>> {
        self.data.iter_mut().map(GraphPointView)
    }

    #[inline] pub fn iter_selection_mut(&mut self) -> impl Iterator<Item=GraphPointView<'_, T>> {
        unsafe{self.data.iter_indices_unchecked_mut(&self.inner.selection)}
            .map(GraphPointView)
    }

    #[inline] pub fn expand_selection(&mut self, ids: impl Iterator<Item = usize>) -> JsResult<()> {
        let len = self.data.len();
        for id in ids {
            js_assert!(id < len)?;
            self.selection.push_sorted(id);
        }
        Ok(())
    }

    /// The function is unsafe because maintaining sorted order of the points is on the caller.
    #[inline] pub unsafe fn insert_point(&mut self, at: usize, point: T) {
        self.redraw = true;
        self.data.insert(at, point);
        SliceMove{from: self.data.len(), to: at}.apply(&mut self.selection)
    }

    /// returns the index at which the new point will be available
    #[inline] pub fn add_point(&mut self, point: T) -> usize {
        self.redraw = true;
        let to = self.data.push_sorted(point);
        SliceMove{from: self.data.len(), to}.apply(&mut self.selection);
        to
    }

    /// `sink` is the function that will be called on every removed point.
    /// If there's nothing to do with the points, standard library's `drop` function can be passed.
    #[inline] pub fn remove_points(
        &mut self,
        to_remove: impl Iterator<Item = usize> + DoubleEndedIterator,
        mut sink: impl FnMut((usize, T))
    ) -> JsResult<()> {
        self.redraw = true;
        let GraphEditor{inner, data} = self;
        let mut ids_iter = inner.selection.iter_mut().rev();
        let mut prev_id = data.len();
        for id in to_remove.rev() {
            js_assert!(id < replace(&mut prev_id, id))?;
            sink((id, data.try_remove(id).to_js_result(loc!())?));
            let rem = loop {
                let Some(x) = ids_iter.next() else {break 0};
                match id.cmp(x) {
                    Ordering::Less =>
                        *x -= 1,
                    Ordering::Equal =>
                        break ids_iter.len().pipe(|x| unsafe{inner.selection.remove_unchecked(x); x}),
                    Ordering::Greater =>
                        break ids_iter.len()
                }
            };
            ids_iter = inner.selection.get_mut(..rem).unwrap_or(&mut []).iter_mut().rev();
        }
        Ok(())
    }

    #[inline] pub fn force_redraw(&mut self) {self.redraw = true}

    /// must be called when a canvas has just been bound or its dimensions have been changed
    pub fn init(&mut self) -> JsResult<()> {
        let canvas: HtmlCanvasElement = self.canvas.cast().to_js_result(loc!())?;
        let [w, h] = T::canvas_coords(&canvas).add_loc(loc!())?;
        canvas.set_width(w);
        canvas.set_height(h);
        self.scale = self.scale.map(|x| x.ceil_to(r64![2.0]));
        self.grid = None;
        if self.offset.x <= 0 {
            self.offset.x = (T::OFFSET_X_BOUND.start * R64::from(w) / self.scale[0]).into();
        }
        if self.offset.y <= 0 {
            self.offset.y = (T::OFFSET_Y_BOUND.start * R64::from(h) / self.scale[1]).into();
        }
        let ctx = canvas.get_2d_context(loc!())?;
        ctx.set_font(AnyGraphEditor::FONT);
        ctx.set_line_width(AnyGraphEditor::LINE_WIDTH);
        Ok(self.redraw = true)
    }

    #[inline] fn point_by_pos(&self, loc: [R64; 2]) -> Option<SliceRef<'_, T>> {
        self.data.iter().enumerate().find(|(_, x)| x.in_hitbox(loc))
            .map(|(id, x)| unsafe{SliceRef::raw(x, id)})
    }

    #[inline] fn point_in_selection(&self, loc: ConfinedAlignedUserPoint) -> bool {
        loc.sub(&self.selection_src).zip_fold(true, self.selection_size, |r, x, y| r && 0.0 <= *x && x <= y)
    }

    #[inline] fn set_zoom_focus(&mut self, ctx: &mut AppContext, cursor: Cursor, loc: impl Deref<Target = [R64; 2]>) {
        let first = !matches!(self.focus, Focus::Plane{..});
        self.focus = Focus::Zoom{init_offset: self.offset,
            pivot: cursor.point + self.offset,
            init_scale: self.scale};
        self.changed_focus = true;
        T::on_plane_hover(self, ctx, cursor, loc, first)
    }

    #[inline] fn set_plane_focus(&mut self, ctx: &mut AppContext, cursor: Cursor, loc: impl Deref<Target = [R64; 2]>) {
        self.focus = Focus::Plane{origin: default(), init_offset: default(), press_time: default()};
        self.changed_focus = true;
        T::on_plane_hover(self, ctx, cursor, loc, true);
    }

    #[inline] fn set_point_focus(&mut self, ctx: &mut AppContext, cursor: Cursor, id: usize) {
        self.focus = Focus::Point{id, last_loc: default(), origin: default(), meta: false};
        self.changed_focus = true;
        T::on_point_hover(self, ctx, cursor, id, true)
    }

    #[inline] fn set_selection_focus(&mut self, ctx: &mut AppContext, cursor: Cursor) {
        self.focus = Focus::Selection{origin: default(), end: default(), meta: false};
        self.changed_focus = true;
        T::on_selection_hover(self, ctx, cursor, true)
    }

    fn handle_hover(&mut self, cursor: Option<Cursor>, ctx: &mut AppContext) -> JsResult<()> {
        let Some(cursor) = cursor else {
            self.focus = Focus::None;
            self.changed_focus = true;
            return Ok(())
        };

        let size = self.canvas.cast::<HtmlCanvasElement>()
            .to_js_result(loc!())?.size();
        let snap_step = [ctx.snap_step(), T::Y_SNAP];
        let step = &R64::array_from(size).div(&self.scale);

        let cursor_point_user = LazyCell::new({
            let off = self.offset;
            move || R64::array_from(cursor.point + off).div(step)
        });
        let cursor_point_user_aligned_confined = LazyCell::new(||
            [T::X_BOUND, T::Y_BOUND].fit(cursor_point_user.floor_to(snap_step)));

        match &mut self.inner.focus {
            Focus::None => self.set_plane_focus(ctx, cursor, Alias(&cursor_point_user_aligned_confined)),

            Focus::Zoom{init_offset, init_scale, pivot} => if cursor.left {
                if !self.inner.last_cursor.left {
                    *pivot = cursor.point + self.inner.offset;
                    *init_scale = self.inner.scale;
                    *init_offset = self.inner.offset;
                } else {
                    self.inner.redraw = true;
                }
                let point = cursor.point + *init_offset - *pivot;
                if !T::SCALE_X_BOUND.is_empty() {
                    self.inner.scale[0] = T::SCALE_X_BOUND.fit(r64![50.0] / size[1] * point.x + init_scale[0]);
                    self.inner.offset.x = ((init_scale[0] - self.inner.scale[0]) * pivot.x / self.inner.scale[0] + init_offset.x).into();
                }
                if !T::SCALE_Y_BOUND.is_empty() {
                    self.inner.scale[1] = T::SCALE_Y_BOUND.fit(r64![50.0] / size[0] * point.y + init_scale[1]);
                    self.inner.offset.y = ((init_scale[1] - self.inner.scale[1]) * pivot.y / self.inner.scale[1] + init_offset.y).into();
                }
            } else {
                if self.inner.last_cursor.left {
                    let offset_delta = self.inner.offset - *init_offset;
                    let scale_delta  = self.inner.scale.sub(init_scale);
                    if !offset_delta.is_zero() || scale_delta.any(|x| **x != 0.0) {
                        ctx.register_action(AppAction::DragPlane{
                            editor_id: self.inner.id,
                            offset_delta, scale_delta})
                    }
                }
                if cursor.shift {
                    *pivot = cursor.point;
                    self.redraw = true;
                } else {
                    self.set_plane_focus(ctx, cursor, Alias(&cursor_point_user_aligned_confined));
                }
            }

            Focus::Plane{origin, init_offset, press_time} => match *cursor {
                Buttons{shift: true, left: false, ..} =>
                    self.set_zoom_focus(ctx, cursor, Alias(&cursor_point_user_aligned_confined)),

                Buttons{left: false, meta, ..} => if self.inner.last_cursor.left {
                    let cur_loc = *cursor_point_user;
                    if self.inner.last_cursor.meta {
                        let origin = *origin;
                        let end = *cursor_point_user_aligned_confined;
                        let area = [origin, end].flipped().map(|x| (x[0] .. x[1]).ordered());
                        let prev_src = replace(&mut self.inner.selection_src, area.clone().map(|x| x.start));
                        let prev_size = replace(&mut self.inner.selection_size, area.clone().map(|x| x.end - x.start));

                        let prev_ids = replace(&mut self.inner.selection, self.data.iter().enumerate()
                            .filter_map(|(id, x)| x.loc().array_check_in(&area).map(|_| id))
                            .collect()).to_box();
                        if let Some(p) = self.point_by_pos(cur_loc) {
                            self.set_point_focus(ctx, cursor, p.index());
                        } else {
                            self.set_plane_focus(ctx, cursor, &end);
                        }

                        let action = T::on_click(self, ctx, cursor,
                            &origin, &end,
                            Some(&prev_ids));
                        ctx.register_action(action.unwrap_or_else(|| AppAction::SetSelection{
                            editor_id: self.id,
                            prev_ids, prev_src, prev_size,
                            cur_ids: self.selection.to_box(), cur_src: self.selection_src, cur_size: self.selection_size}))
                    } else if *ctx.now() - **press_time > 0.33 {
                        let init_offset = *init_offset;
                        let action = T::on_click(self, ctx, cursor,
                            Alias(&cursor_point_user_aligned_confined), Alias(&cursor_point_user_aligned_confined),
                            None);

                        if let Some(offset_delta) = (self.inner.offset - init_offset).nonzero() {
                            ctx.register_action(action.unwrap_or(AppAction::DragPlane{
                                editor_id: self.inner.id,
                                offset_delta, scale_delta: default()}))
                        }
                    } else {
                        let init_offset = *init_offset;
                        let prev_ids = take(&mut self.selection).into_boxed_slice();
                        let prev_size = take(&mut self.selection_size);
                        let action = T::on_click(self, ctx, cursor,
                            Alias(&cursor_point_user_aligned_confined), Alias(&cursor_point_user_aligned_confined),
                            Some(&prev_ids));

                        if !prev_ids.is_empty() {
                            ctx.register_action(AppAction::SetSelection{
                                editor_id: self.id,
                                prev_ids, prev_size, prev_src: self.selection_src,
                                cur_ids: self.selection.to_box(), cur_size: default(), cur_src: self.selection_src})
                        }
                        if let Some(action) = action {
                            ctx.register_action(action)
                        } else if let Some(offset_delta) = (self.offset - init_offset).nonzero() {
                            ctx.register_action(AppAction::DragPlane{
                                editor_id: self.id,
                                offset_delta,
                                scale_delta: default()});
                        }
                    }
                } else if self.point_in_selection(*cursor_point_user) {
                    self.set_selection_focus(ctx, cursor);
                } else if let Some(p) = self.point_by_pos(*cursor_point_user) {
                    self.set_point_focus(ctx, cursor, p.index());
                } else {
                    self.redraw |= self.last_cursor.meta | meta;
                }

                Buttons{left: true, meta, ..} => if meta {
                    if !self.inner.last_cursor.left {
                        *origin = *cursor_point_user_aligned_confined;
                    } else {self.inner.redraw = true}
                } else {
                    if !self.inner.last_cursor.left {
                        *press_time = ctx.now();
                        *init_offset = self.inner.offset;
                    } else {self.inner.redraw = true}

                    if !T::OFFSET_X_BOUND.is_empty() {
                        self.inner.offset.x = T::OFFSET_X_BOUND.map(|x| x * step[0])
                            .extend(self.inner.offset.x).fit(self.inner.offset.x + self.inner.last_cursor.point.x - cursor.point.x);
                    }
                    if !T::OFFSET_Y_BOUND.is_empty() {
                        self.inner.offset.y = T::OFFSET_Y_BOUND.map(|y| y * step[1])
                            .extend(self.inner.offset.y).fit(self.inner.offset.y + self.inner.last_cursor.point.y - cursor.point.y);
                    }
                }
            }

            Focus::Point{id, last_loc, origin, meta} => if cursor.left {
                let delta = if !self.inner.last_cursor.left {
                    *last_loc = *cursor_point_user_aligned_confined;
                    *origin = *last_loc;
                    *meta = cursor.meta;
                    default()
                } else {
                    let new = *cursor_point_user_aligned_confined;
                    new.sub(&replace(last_loc, new))
                };
                if *delta.sum::<R64>() != 0.0 {
                    T::move_point(Ok(unsafe{self.data.get_unchecked_mut(*id)}), delta, *meta);
                    unsafe{self.data.reorder_unchecked(*id)}.apply(from_mut(id));
                    self.redraw = true;
                    T::on_move(self, ctx, cursor, delta)
                }
            } else if self.inner.last_cursor.left {
                let (meta, src, point_id) = (*meta, *origin, *id);
                let prev_ids = replace(&mut self.selection, vec![point_id]).into_boxed_slice();
                let action = T::on_click(self, ctx, cursor,
                    &src, Alias(&cursor_point_user_aligned_confined),
                    Some(&prev_ids));

                if self.selection[..] != prev_ids[..] {
                    ctx.register_action(AppAction::SetSelection{
                        editor_id: self.id,
                        prev_ids, prev_src: self.selection_src, prev_size: take(&mut self.selection_size),
                        cur_ids: self.selection.to_box(), cur_src: self.selection_src, cur_size: default()});
                }
                if let Some(action) = action {
                    ctx.register_action(action)
                } else {
                    let delta = cursor_point_user_aligned_confined.sub(&src);
                    if delta.any(|x| **x != 0.0) {
                        ctx.register_action(AppAction::DragPoint{
                            editor_id: self.id, point_id, delta, meta});
                    }
                }
            } else if cursor.shift {
                self.set_zoom_focus(ctx, cursor, Alias(&cursor_point_user_aligned_confined))
            } else if !unsafe{self.data.get_unchecked(*id)}.in_hitbox(*cursor_point_user) {
                self.set_plane_focus(ctx, cursor, Alias(&cursor_point_user_aligned_confined))
            }

            Focus::Selection{origin, end, meta} => if cursor.left {
                let delta = if !self.inner.last_cursor.left {
                    *end = *cursor_point_user_aligned_confined;
                    *origin = *end;
                    *meta = cursor.meta;
                    default()
                } else {
                    let new = *cursor_point_user_aligned_confined;
                    new.sub(&replace(end, new))
                };
                if *delta.sum::<R64>() != 0.0 {
                    self.inner.redraw = true;
                    for (ids, id) in self.inner.selection.iter_mut_with_ctx() {
                        T::move_point(Ok(unsafe{self.data.get_unchecked_mut(id)}), delta, *meta);
                        unsafe{self.data.reorder_unchecked(id)}.apply(ids);
                    }
                    T::move_point(Err(&mut self.inner.selection_src), delta, *meta);
                    self.selection.sort_unstable();
                    T::on_move(self, ctx, cursor, delta)
                }
            } else if self.inner.last_cursor.left {
                let (meta, src) = (*meta, take(origin));
                *end = default();
                let action = T::on_click(self, ctx, cursor,
                    &src, Alias(&cursor_point_user_aligned_confined),
                    None);

                ctx.register_action(action.unwrap_or_else(|| AppAction::DragSelection{
                    editor_id: self.id,
                    delta: cursor_point_user_aligned_confined.sub(&src),
                    meta}))
            } else if cursor.shift {
                self.set_zoom_focus(ctx, cursor, Alias(&cursor_point_user_aligned_confined))
            } else if !self.point_in_selection(*cursor_point_user) {
                if let Some(p) = self.point_by_pos(*cursor_point_user) {
                    self.set_point_focus(ctx, cursor, p.index())
                } else {
                    self.set_plane_focus(ctx, cursor, Alias(&cursor_point_user_aligned_confined))
                }
            }
        };

        if self.changed_focus {
            self.changed_focus = false
        } else {
            match self.focus {
                Focus::None => (),
                Focus::Zoom{..} | Focus::Plane{..} =>
                    T::on_plane_hover(self, ctx, cursor, cursor_point_user_aligned_confined, false),
                Focus::Point{id, ..} =>
                    T::on_point_hover(self, ctx, cursor, id, false),
                Focus::Selection{..} =>
                    T::on_selection_hover(self, ctx, cursor, false)
            }
        }

        let old_buttons = *replace(&mut self.last_cursor, cursor);
        self.redraw |= *cursor != old_buttons;
        Ok(())
    }

    /// an offset of 0 is assumed
    /// the returned array are the actual bounds of the rendered grid in user coordinates
    fn draw_grid(canvas_size: [R64; 2], step: [R64; 2], scale: [R64; 2]) -> JsResult<(Path2d, [R64; 2])> {
        let res = Path2d::new().add_loc(loc!())?;
        let steps: [usize; 2] = [T::X_BOUND.end, T::Y_BOUND.end].mul(&step)
            .zip(canvas_size, min).div(&canvas_size).mul(&scale)
            .into_array();

        for x in 0 .. steps[0] {
            for y in (0 .. steps[1]).step_by(2) {
                let [x, y] = [step[0] * x, step[1] * y];
                res.rect(*x + AnyGraphEditor::LINE_WIDTH, *y, *step[0] - AnyGraphEditor::LINE_WIDTH, *step[1]);
                res.rect(*x, *y + *step[1], AnyGraphEditor::LINE_WIDTH, *step[1]);
            }
        }

        Ok((res, scale))
    }

    pub fn handle_event(&mut self, event: &AppEvent, ctx: &mut AppContext) -> JsResult<()> {
        Ok(match event {
            AppEvent::Enter(id, e) | AppEvent::Hover(id, e) if *id == self.id =>
                self.handle_hover(Some(e.try_into().add_loc(loc!())?), ctx).add_loc(loc!())?,

            AppEvent::Focus(id, e) if *id == self.id => {
                e.target_dyn_into::<Element>().to_js_result(loc!())?
                    .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                self.handle_hover(Some(e.try_into().add_loc(loc!())?), ctx).add_loc(loc!())?;
            }

            AppEvent::KeyPress(id, e) | AppEvent::KeyRelease(id, e) if *id == self.id =>
                self.handle_hover(Some(self.last_cursor + e), ctx).add_loc(loc!())?,

            AppEvent::Leave(id) if *id == self.id =>
                self.handle_hover(None, ctx).add_loc(loc!())?,

            AppEvent::Resize =>
                self.init().add_loc(loc!())?,

            AppEvent::AudioStarted(_) => self.redraw = true,

            AppEvent::Undo(actions) => for action in actions.iter() {
                match *action {
                    AppAction::DragPlane{editor_id, offset_delta, scale_delta} => if editor_id == self.id {
                        self.redraw  = true;
                        self.offset -= offset_delta;
                        self.scale   = self.scale.sub(&scale_delta);
                    }

                    AppAction::DragPoint{editor_id, point_id, mut delta, meta} => if editor_id == self.id {
                        self.redraw = true;
                        delta = delta.map(|x| -x);
                        T::move_point(Ok(unsafe{self.data.get_unchecked_mut(point_id)}), delta, meta)
                    }

                    AppAction::DragSelection{editor_id, mut delta, meta} => if editor_id == self.id {
                        self.redraw = true;
                        delta = delta.map(|x| -x);
                        for &id in self.inner.selection.iter() {
                            T::move_point(Ok(unsafe{self.data.get_unchecked_mut(id)}), delta, meta);
                        }
                        T::move_point(Err(&mut self.selection_src), delta, meta)
                    }

                    AppAction::SetSelection{editor_id, ref prev_ids, prev_src, prev_size, ..} => if editor_id == self.id {
                        self.redraw = true;
                        self.selection = prev_ids.to_vec();
                        self.selection_src = prev_src;
                        self.selection_size = prev_size;
                    }

                    _ => T::on_undo(self, ctx, action)
                }
            }

            AppEvent::Redo(actions) => for action in actions.iter() {
                match *action {
                    AppAction::DragPlane{editor_id, offset_delta, scale_delta} => if editor_id == self.id {
                        self.redraw  = true;
                        self.offset += offset_delta;
                        self.scale   = self.scale.add(&scale_delta);
                    }

                    AppAction::DragPoint{editor_id, point_id, delta, meta} => if editor_id == self.id {
                        self.redraw = true;
                        T::move_point(Ok(unsafe{self.data.get_unchecked_mut(point_id)}), delta, meta)
                    }

                    AppAction::DragSelection{editor_id, delta, meta} => if editor_id == self.id {
                        self.redraw = true;
                        for &id in self.inner.selection.iter() {
                            T::move_point(Ok(unsafe{self.data.get_unchecked_mut(id)}), delta, meta);
                        }
                        T::move_point(Err(&mut self.selection_src), delta, meta)
                    }

                    AppAction::SetSelection{editor_id, ref cur_ids, cur_src, cur_size, ..} => if editor_id == self.id {
                        self.redraw = true;
                        self.selection = cur_ids.to_vec();
                        self.selection_src = cur_src;
                        self.selection_size = cur_size;
                    }

                    _ => T::on_redo(self, ctx, action)
                }
            }

            AppEvent::Frame(_) if self.redraw => {
                let canvas: HtmlCanvasElement = self.canvas().cast().to_js_result(loc!())?;
                let size = canvas.size().map(R64::from);
                let snap_step = [ctx.snap_step(), T::Y_SNAP];
                let canvas_ctx = canvas.get_2d_context(loc!())?;

                let step = &size.div(&self.scale);
                let offset_x = R64::from(-self.offset.x)
                    % (self.offset.x > (T::X_BOUND.start * step[0]).into())
                        .choose(step[0], R64::INFINITY);
                let offset_y = R64::from(-self.offset.y)
                    % (self.offset.y > (T::Y_BOUND.start * step[1]).into())
                    .choose(step[1] * 2.0, R64::INFINITY);

                let offset = |loc| loc + self.inner.offset;
                let to_user = |loc| R64::array_from(offset(loc)).div(step);
                let to_aligned_canvas = |loc: Point| loc.floor_to(snap_step.mul(step).into());
                let confine = |x| [T::X_BOUND, T::Y_BOUND].fit(x);

                canvas_ctx.set_fill_style(&AnyGraphEditor::BG_STYLE.into());
                canvas_ctx.fill_rect(0.0, 0.0, *size[0], *size[1]);

                let (grid, original_scale) = self.inner.grid.get_or_try_insert(||
                    Self::draw_grid(size, *step, self.inner.scale).add_loc(loc!()))?;
                let grid_scale = original_scale.div(&self.inner.scale);
                let repetitions = self.inner.scale.sub(&[offset_x, offset_y].div(step))
                    .div(original_scale)
                    .map(|x| usize::from(x.ceil()));

                canvas_ctx.set_fill_style(&AnyGraphEditor::MG_STYLE.into());
                canvas_ctx.transform(*grid_scale[0], 0.0, 0.0, *grid_scale[1], *offset_x, *offset_y).add_loc(loc!())?;

                for _ in 0 .. repetitions[1] {
                    for _ in 0 .. repetitions[0] {
                        canvas_ctx.fill_with_path_2d(grid);
                        canvas_ctx.translate(*size[0], 0.0).add_loc(loc!())?;
                    }
                    canvas_ctx.translate(-*size[0] * repetitions[0] as f64, *size[1]).add_loc(loc!())?;
                }
                canvas_ctx.reset_transform().add_loc(loc!())?;

                let points = Path2d::new().add_loc(loc!())?;
                let mapper = |loc: [R64; 2]| loc.mul(step).sub(&R64::array_from(self.inner.offset));
                for [this, next] in self.data.array_windows::<2>() {
                    points.add_path(&this.draw(Some(next), mapper).add_loc(loc!())?);
                }
                if let Some(last) = self.data.last() {
                    points.add_path(&last.draw(None, mapper).add_loc(loc!())?);
                }
                canvas_ctx.fill_with_path_2d(&points);
                canvas_ctx.set_stroke_style(&AnyGraphEditor::FG_STYLE.into());
                canvas_ctx.stroke_with_path(&points);

                let [x, y] = mapper(self.selection_src);
                let [w, h] = self.selection_size.mul(step);
                canvas_ctx.set_line_dash(eval_once!(JsValue: js_array![number 10.0, number 10.0])).add_loc(loc!())?;
                canvas_ctx.stroke_rect(*x, *y, *w, *h);
                canvas_ctx.set_line_dash(eval_once!(JsValue: js_array![])).add_loc(loc!())?;

                if ctx.play_since().is_finite() {
                    canvas_ctx.set_fill_style(&AnyGraphEditor::FG_STYLE.into());
                    let play_at = (ctx.now() - ctx.play_since()).secs_to_beats(ctx.bps());
                    canvas_ctx.fill_rect(*play_at * *step[0] - self.offset.x as f64, 0.0, 3.0, *size[1]);
                } else {
                    self.inner.redraw = false;
                }

                match self.focus {
                    Focus::Zoom{pivot, init_offset, ..} => if self.last_cursor.left {
                        let [x, y] = (pivot - init_offset).map(|x| x as f64);
                        canvas_ctx.begin_path();
                        canvas_ctx.move_to(x - 10.0, y);
                        canvas_ctx.line_to(x + 10.0, y);
                        canvas_ctx.move_to(x, y - 10.0);
                        canvas_ctx.line_to(x, y + 10.0);
                        canvas_ctx.set_stroke_style(&AnyGraphEditor::FG_STYLE.into());
                        canvas_ctx.stroke();
                    } else {
                        canvas_ctx.set_text_align("left");
                        canvas_ctx.set_text_baseline("bottom");
                        canvas_ctx.set_fill_style(&AnyGraphEditor::FG_STYLE.into());
                        canvas_ctx.fill_text(&T::fmt_loc(confine(to_user(pivot))), 5.0, *size[1] - 5.0)
                            .add_loc(loc!())?;
                    }

                    Focus::Plane{origin, ..} if self.last_cursor.meta => if self.last_cursor.left {
                        let cur = to_aligned_canvas(self.last_cursor.point);
                        let origin = mapper(origin).map(|x| *x);
                        canvas_ctx.set_line_dash(eval_once!(JsValue: js_array![number 10.0, number 10.0])).add_loc(loc!())?;
                        canvas_ctx.stroke_rect(origin[0], origin[1], cur.x as f64 - origin[0], cur.y as f64 - origin[1]);
                        canvas_ctx.set_line_dash(eval_once!(JsValue: js_array![])).add_loc(loc!())?;
                    } else {
                        let [x, y] = self.last_cursor.point.map(|x| x as f64);
                        canvas_ctx.set_line_dash(eval_once!(JsValue: js_array![number 10.0, number 10.0])).add_loc(loc!())?;
                        canvas_ctx.begin_path();
                        canvas_ctx.move_to(-*size[0], y);
                        canvas_ctx.line_to( *size[0], y);
                        canvas_ctx.move_to(x, -*size[1]);
                        canvas_ctx.line_to(x,  *size[1]);
                        canvas_ctx.stroke();
                        canvas_ctx.set_line_dash(eval_once!(JsValue: js_array![])).add_loc(loc!())?;
                    }
                    
                    _ => ()
                }
            }

            _ => (),
        })
    }
}
