use std::{
    iter::{Iterator, successors as succ},
    slice::from_raw_parts,
    mem::{Discriminant, discriminant, take},
    ops::{Not, Div, Range, Deref},
    fmt::Debug, rc::Rc, borrow::Cow};
use web_sys::{
    HtmlCanvasElement,
    AnalyserNode,
    ImageData,
    MouseEvent,
    HtmlElement,
    Path2d, PointerEvent, AudioContext, SvgElement, Element};
use wasm_bindgen::{Clamped, JsValue, JsCast};
use yew::{TargetCast, NodeRef};
use crate::{
    utils::{Check, SliceExt, Point,
        JsResult, HtmlCanvasExt, JsResultUtils, OptionExt,
        HtmlElementExt, 
        Pipe, Tee, RatioToInt, BoolExt, RangeExt, VecExt, R64},
    sound::FromBeats,
    global::{AppEvent, AppContext},
    loc,
    r64, js_assert,
};

pub struct EveryNth<'a, T> {
    iter: &'a [T],
    n: usize,
    state: usize,
    off: usize
}

impl<'a, T> Iterator for EveryNth<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
		if let res @Some(_) = self.iter.get(self.state) {
            self.state += self.n;
            res
        } else {
            self.off += 1;
            if self.off == self.n {
                None
            } else {
                self.state = self.off + self.n;
                self.iter.get(self.state - self.n)
            }
        }
    }
}

pub struct EveryNthMut<'a, T> {
    iter: &'a mut [T],
    n: usize,
    state: usize,
    off: usize
}

impl<'a, T> Iterator for EveryNthMut<'a, T> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
		if let Some(res) = self.iter.get_mut(self.state) {
            self.state += self.n;
            Some(unsafe{(res as *mut T).as_mut().unwrap_unchecked()})
        } else {
            self.off += 1;
            if self.off == self.n {
                None
            } else {
                self.state = self.off + self.n;
                self.iter.get_mut(self.state - self.n)
                    .map(|x| unsafe{(x as *mut T).as_mut().unwrap_unchecked()})
            }
        }
    }
}

pub trait ToEveryNth<T> {
    fn every_nth(&self, n: usize) -> EveryNth<'_, T>;
    fn every_nth_mut(&mut self, n: usize) -> EveryNthMut<'_, T>;
}

impl<T> ToEveryNth<T> for [T] {
    #[inline] fn every_nth(&self, n: usize) -> EveryNth<'_, T> {
        EveryNth {iter: self, n, state: 0, off: 0}
    }
    #[inline] fn every_nth_mut(&mut self, n: usize) -> EveryNthMut<'_, T> {
        EveryNthMut {iter: self, n, state: 0, off: 0}
    }
}

#[test]
fn test_every_nth_mut() {
    let mut data = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    let transposed: Vec<u8>     = data.every_nth(3).copied().collect();
    let transposed_mut: Vec<u8> = data.every_nth_mut(3).map(|x| *x).collect();
    assert_eq!(transposed,     [0, 3, 6, 9, 1, 4, 7, 10, 2, 5, 8]);
    assert_eq!(transposed_mut, [0, 3, 6, 9, 1, 4, 7, 10, 2, 5, 8]);
}


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

            AppEvent::Frame(..) if ctx.play_since.is_finite() => {
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


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CanvasEvent {
    pub point: Point,
    pub left: bool,
    pub shift: bool
}

impl TryFrom<&MouseEvent> for CanvasEvent {
    type Error = JsValue;
    fn try_from(value: &MouseEvent) -> Result<Self, Self::Error> {
        let canvas: HtmlCanvasElement = value.target_dyn_into().to_js_result(loc!())?;
        let point = Point{x: value.offset_x(), y: value.offset_y()}
            .normalise(canvas.client_rect(), canvas.rect());
        Ok(Self{point, left: value.buttons() & 1 == 1, shift: value.shift_key()})
    }
}

impl TryFrom<&PointerEvent> for CanvasEvent {
    type Error = JsValue;
    fn try_from(value: &PointerEvent) -> Result<Self, Self::Error> {
        let canvas: HtmlCanvasElement = value.target_dyn_into().to_js_result(loc!())?;
        let point = Point{x: value.offset_x(), y: value.offset_y()}
            .normalise(canvas.client_rect(), canvas.rect());
        Ok(Self{point, left: value.buttons() & 1 == 1, shift: value.shift_key()})
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
                        break Default::default()
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
pub trait Graphable: Debug {
    /// the name of the plane that will be displayed as a hint when hovered over it
    const EDITOR_NAME: &'static str;
    /// bounds for the scale of the X axis of the graph
    const SCALE_X_BOUND: Range<R64>;
    /// bounds for the scale of the Y axis of the graph
    const SCALE_Y_BOUND: Range<R64>;
    /// type of the inner data of the point, can be whatever and,
    /// unlike the coordinates, can be freely mutated
    type Inner;
    /// type returned from `GraphEditor::handle_hover` when the user interacted with the UI in a special way,
    /// i.e. selected a point or dropped in a `Draggable`
    type Event = ();
    /// type passed to `GraphEditor::handle_drag_n_drop` when the user started dragging an item
    /// into the editor space
    type Draggable: Copy + Debug = ();

    /// inner data of the point
    fn inner(&self) -> &Self::Inner;
    /// mutable inner data of the point
    fn inner_mut(&mut self) -> &mut Self::Inner;
    /// location of the point in user coordinates
    fn loc(&self) -> [R64; 2];
    /// set the location of the point in user coordinates when moved in the UI
    /// can optionally emit an event
    fn set_loc(&mut self, n_points: usize, self_id: usize, x: impl FnOnce() -> R64, y: impl FnOnce() -> R64)
    -> Option<Self::Event>;
    /// returns `true` if the given user coordinates are inside the hitbox of the point
    fn in_hitbox(&self, point: [R64; 2]) -> bool;

    /// the event to emit when the point is selected/de-selected in the UI
    #[allow(unused_variables)] #[inline]
    fn on_select(self_id: Option<usize>) -> Option<Self::Event> {None}
    /// the event to emit when a `Draggable` is dropped into the UI
    #[allow(unused_variables)] #[inline]
    fn on_drop_in(value: Self::Draggable, loc: impl FnOnce() -> [R64; 2]) -> Option<Self::Event> {None}

    /// visual representation of the point's hitbox
    /// `mapper` maps user coordinates to canvas coordinates, all vertices in the
    /// returned `Path2d` must be the result of the `mapper` function
    fn draw(&self, next: Option<&Self>, mapper: impl Fn([f64; 2]) -> [f64; 2]) -> JsResult<Path2d>;
    // return type of `desc` and `fmt_loc` should be `impl AsRef<str>`
    // but feature `return_position_impl_trait_in_trait` is not usable yet
    /// description of the point that'll be shown in the hint handler when the point's hovered over
    #[allow(unused_variables)] #[inline]
    fn desc(&self) -> String {String::new()}
    /// format the location of a point in user coordinates
    fn fmt_loc(loc: [R64; 2]) -> String;
    /// visual representation of `Draggable`, evaluated every time `Draggable` enters the UI
    /// the center of the representation should be 0, it will then be moved accordingly
    #[allow(unused_variables)] #[inline]
    fn draw_draggable(value: Self::Draggable, step: [R64; 2]) -> JsResult<Path2d> {Path2d::new()}
    /// the hint to return when a `Draggable` is dragged over the UI but not yet dropped in
    /// `loc` returns the location of the draggable in user coordinates if its inside the editor,
    /// otherwise it returns `None`
    #[allow(unused_variables)] #[inline]
    fn drag_n_drop_hint(d: Self::Draggable, loc: impl FnOnce() -> Option<[R64; 2]>) -> Option<[Cow<'static, str>; 2]> {None}
}

/// a special reference wrapper: access to everything is immutable,
/// except for the inner value, which is mutable
pub struct GraphPointView<'a, T: Graphable>(&'a mut T);

impl<'a, T: Graphable> Deref for GraphPointView<'a, T> {
    type Target = T;
    #[inline] fn deref(&self) -> &Self::Target {self.0}
}

impl<'a, T: Graphable> GraphPointView<'a, T> {
    #[inline] pub fn inner(&mut self) -> &mut T::Inner {
        self.0.inner_mut()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
enum Focus<D: Debug> {
    #[default] None,
    HoverPlane,
    HoverPoint(usize),
    MovePlane(Point),
    MovePoint(usize),
    AwaitDragIn(D),
    HoverWDraggable(D, Point, Path2d),
    HoverPlaneWShift(Point),
    ZoomPlane{init_offset: Point, pivot: Point, init_scale_x: R64, init_scale_y: R64}
}

impl<D: Copy + Debug> Focus<D> {
    #[inline] pub fn draggable(self) -> Option<D> {
        if let Focus::AwaitDragIn(d)
            | Focus::HoverWDraggable(d, ..) = self {Some(d)}
        else {None}
    }

    #[inline] pub fn always_update(&self) -> bool {
        matches!(self, Self::MovePoint{..}
            | Self::ZoomPlane{..}
            | Self::HoverPlaneWShift(..)
            | Self::HoverWDraggable(..))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GraphEditor<T: Graphable> {
    canvas: NodeRef,
    offset: Point,
    scale_x: R64,
    scale_y: R64,
    focus: Focus<T::Draggable>,
    last_focus: Discriminant<Focus<T::Draggable>>,
    redraw: bool,
    data: Vec<T>,
    fixed: Option<usize>
}

impl<T: Graphable> GraphEditor<T> {
    const FONT: &str = "20px consolas";
    const BG_STYLE: &str = "#232328";
    const MG_STYLE: &str = "#333338";
    const FG_STYLE: &str = "#0069E1";
    const LINE_WIDTH: f64 = 3.0;

    #[inline] pub fn new(scale_x: R64, scale_y: R64, data: Vec<T>) -> Self {
        Self{canvas: NodeRef::default(), offset: Point::ZERO,
            focus: Focus::None, last_focus: discriminant(&Focus::HoverPlane),
            scale_x, scale_y,
            redraw: false, data, fixed: None}
    }

    #[inline] pub fn canvas(&self) -> &NodeRef {&self.canvas}

    #[inline] pub fn data(&self) -> &[T] {&self.data}

    #[inline] pub unsafe fn get_unchecked_mut(&mut self, index: usize)
    -> GraphPointView<'_, T> {
        GraphPointView(self.data.get_unchecked_mut(index))
    }

    #[inline] pub fn iter_mut(&mut self) -> impl Iterator<Item=GraphPointView<'_, T>> {
        self.data.iter_mut().map(GraphPointView)
    }

    /// the item will be accessible as the last element
    #[inline] pub fn add_point(&mut self, point: T) -> usize {
        self.redraw = true;
        let id = self.data.push_sorted_by_key(point, |x| x.loc()[0]);
        if let Some(ref mut x) = self.fixed.filter(|x| *x >= id) {
            *x += 1
        }
        id
    }

    /// the element at `index`, if `Some`, will be returned from subsequent calls to `fixed(_mut)` 
    #[inline] pub fn fix_point(&mut self, index: Option<usize>) -> JsResult<()> {
        if let Some(index) = index {js_assert!(index < self.data.len())?}
        Ok(self.fixed = index)
    }

    /// modify the point at `index` freely in a closure through a mutable reference
    /// unlike `get_mut`, this method allows moving the point
    #[inline] pub fn modify_point(&mut self, index: usize, f: impl FnOnce(&mut T)) -> JsResult<()> {
        let point = self.data.get_mut(index).to_js_result(loc!())?;
        f(point);
        unsafe{self.data.reorder_unchecked_by_key(index, |x| x.loc()[0])};
        Ok(())
    }

    #[inline] pub fn fixed_id(&self) -> Option<usize> {
        self.fixed
    }

    #[inline] pub fn del_fixed(&mut self) -> Option<()> {
        self.fixed.take().map(|x|
            _ = unsafe{self.data.remove_unchecked(x)})
    }

    #[inline] pub fn fixed(&self) -> Option<&T> {
        self.fixed.map(|x| unsafe{self.data.get_unchecked(x)})
    }

    #[inline] pub fn fixed_mut(&mut self) -> Option<GraphPointView<'_, T>> {
        self.fixed.map(|x| unsafe{self.get_unchecked_mut(x)})
    }

    #[inline] pub fn force_redraw(&mut self) {self.redraw = true}

    #[inline] pub fn set_draggable(&mut self, value: Option<T::Draggable>) {
        self.set_focus(value.map_or(Focus::None, Focus::AwaitDragIn));
    }

    /// must be called when a canvas has just been bound or its dimensions have been changed
    pub fn init(&mut self, resizer: impl FnOnce(&HtmlCanvasElement) -> JsResult<[u32; 2]>) -> JsResult<()> {
        let canvas: HtmlCanvasElement = self.canvas.cast().to_js_result(loc!())?;
        let [w, h] = resizer(&canvas).add_loc(loc!())?;
        canvas.set_width(w);
        canvas.set_height(h);
        let ctx = canvas.get_2d_context(loc!())?;
        ctx.set_font(Self::FONT);
        ctx.set_line_width(Self::LINE_WIDTH);
        Ok(self.redraw = true)
    }

    /// the returned IDs are (back, front)
    #[inline] fn id_by_pos(&self, x: R64, y: R64) -> Option<usize> {
        self.data.iter().position(|point| point.in_hitbox([x, y]))
    }

    #[inline] fn set_focus(&mut self, focus: Focus<T::Draggable>) {
        self.focus = focus;
        self.redraw = true;
    }

    pub fn handle_hover(&mut self, event: Option<CanvasEvent>, ctx: &AppContext) -> JsResult<Option<T::Event>> {
        let Some(mut event) = event else {
            self.focus = take(&mut self.focus)
                .draggable().map_or(Focus::None, Focus::AwaitDragIn);
            self.redraw = true;
            return Ok(None)
        };

        event.point += self.offset;
        let [w, h]   = self.canvas.cast::<HtmlCanvasElement>()
            .to_js_result(loc!())?.size();
        let step_x   = R64::from(w) / self.scale_x;
        let step_y   = R64::from(h) / self.scale_y;
        let x        = R64::from(event.point.x) / step_x;
        let y        = R64::from(event.point.y) / step_y;

        let zoom_plane_focus = || Focus::ZoomPlane{
            init_offset: self.offset, pivot: event.point,
            init_scale_x: self.scale_x, init_scale_y: self.scale_y};

        Ok(match self.focus {
            Focus::None => self.set_focus(Focus::HoverPlane)
                .pipe(|_| None),

            Focus::HoverPlane => match (event.left, event.shift) {
                (true, true) =>
                    Some(zoom_plane_focus()),
                (true, false) =>
                    self.id_by_pos(x, y)
                        .map_or_else(|| Focus::MovePlane(event.point - self.offset),
                            Focus::MovePoint).into(),
                (false, true) =>
                    Some(Focus::HoverPlaneWShift(event.point)),
                (false, false) =>
                    self.id_by_pos(x, y).map(Focus::HoverPoint)
            }.map(|x| self.set_focus(x)).pipe(|_| None),

            Focus::HoverPoint(id) => match (event.left, event.shift) {
                (true, true) => Some(zoom_plane_focus()),
                (true, false) => Some(Focus::MovePoint(id)),
                (false, true) => Some(Focus::HoverPlaneWShift(event.point)),
                (false, false) =>
                    unsafe{self.data.get_unchecked(id)}.in_hitbox([x, y])
                        .not().then_some(Focus::HoverPlane),
            }.map(|x| self.set_focus(x)).pipe(|_| None),

            Focus::MovePlane(ref mut last) => if event.left {
                event.point -= self.offset;
                self.offset.x = (self.offset.x + last.x - event.point.x).max(self.offset.x.min(-5));
                self.offset.y = (self.offset.y + last.y - event.point.y).max(self.offset.y.min(-5));
                *last = event.point;
                None
            } else {
                self.focus = Focus::HoverPlane;
                T::on_select(None)
            }.tee(|_| self.redraw = true),

            Focus::MovePoint(id) => if event.left {
                let n_points = self.data.len();
                let point = unsafe{self.data.get_unchecked_mut(id)};
                let res = point.set_loc(n_points, id,
                    || R64::from(event.point.x).div(step_x)
                        .max(r64![0.0]).floor_to(ctx.snap_step),
                    || (R64::from(event.point.y) / step_y).max(r64![0.0]));
                let new_id = unsafe{self.data.reorder_unchecked_by_key(id, |x| x.loc()[0])};
                if new_id != id {
                    self.focus = Focus::MovePoint(new_id);
                    if let Some(ref mut x) = self.fixed.filter(|x| *x == id) {
                        *x = new_id;
                    } else if let Some(ref mut x) = self.fixed.filter(|x| new_id < id && *x < id && *x >= new_id) {
                        *x += 1;
                    } else if let Some(ref mut x) = self.fixed.filter(|x| new_id > id && *x < new_id && *x >= id) {
                        *x -= 1;
                    }
                }
                res
            } else {
                self.focus = Focus::HoverPoint(id);
                T::on_select(Some(id))
            }.tee(|_| self.redraw = true),

            Focus::HoverPlaneWShift(ref mut point) => match (event.left, event.shift) {
                (true, true) => 
                    self.focus = zoom_plane_focus(),
                (true, false) =>
                    self.focus = self.id_by_pos(x, y)
                        .map_or_else(|| Focus::MovePlane(event.point - self.offset),
                            Focus::MovePoint),
                (false, true) => *point = event.point,
                (false, false) => self.focus = Focus::HoverPlane,
            }.pipe(|_| {self.redraw = true; None}),

            Focus::AwaitDragIn(d) => if event.left {
                self.focus = Focus::HoverWDraggable(d, event.point,
                    T::draw_draggable(d, [step_x, step_y]).add_loc(loc!())?);
                None
            } else {
                self.focus = Focus::HoverPlane;
                T::on_drop_in(d, || [R64::from(event.point.x) / step_x, R64::from(event.point.y) / step_y])
            }.tee(|_| self.redraw = true),

            Focus::HoverWDraggable(d, ref mut point, _) => if event.left {
                *point = event.point;
                point.x -= (*ctx.snap_step != 0.0).then_or(0, || point.x % (*step_x * *ctx.snap_step) as i32);
                point.y = point.y - point.y % *step_y as i32 + (*step_y / 2.0) as i32;
                None
            } else {
                self.focus = Focus::HoverPlane;
                T::on_drop_in(d, || [R64::from(event.point.x) / step_x, R64::from(event.point.y) / step_y])
            }.tee(|_| self.redraw = true),

            Focus::ZoomPlane{init_offset, init_scale_x, init_scale_y, pivot} => match (event.left, event.shift) {
                (true, _) => {
                    event.point -= self.offset - init_offset;

                    self.scale_x = T::SCALE_X_BOUND.fit(r64![50.0] / h * (event.point.x - pivot.x) + init_scale_x);
                    self.offset.x = ((init_scale_x - self.scale_x) * pivot.x / self.scale_x + init_offset.x)
                        .to_int();

                    self.scale_y = T::SCALE_Y_BOUND.fit(r64![50.0] / w * (event.point.y - pivot.y) + init_scale_y);
                    self.offset.y = ((init_scale_y - self.scale_y) * pivot.y / self.scale_y + init_offset.y)
                        .to_int();
                }

                (false, true) => self.focus = Focus::HoverPlaneWShift(event.point),

                (false, false) => self.focus = Focus::HoverPlane
            }.pipe(|_| {self.redraw = true; None}),
        })
    }

    pub fn redraw(&mut self, app_ctx: &AppContext) -> JsResult<Option<[Cow<'static, str>; 2]>> {
        if !self.redraw {return Ok(None)}

        let canvas: HtmlCanvasElement = self.canvas().cast().to_js_result(loc!())?;
        let [w, h] = canvas.size().map(R64::from);
        let ctx = canvas.get_2d_context(loc!())?;

        ctx.set_fill_style(&Self::BG_STYLE.into());
        ctx.fill_rect(0.0, 0.0, *w, *h);

        let step_x = w / self.scale_x;
        let step_y = h / self.scale_y;
        let offset_x = R64::from(-self.offset.x)
            % (self.offset.x > 0).choose(step_x, R64::INFINITY);
        let offset_y = R64::from(-self.offset.y)
            % (self.offset.y > 0).choose(step_y * 2.0, R64::INFINITY);

        // lighter horizontal bars
        ctx.begin_path();
        for y in succ(Some(offset_y), |y| (y + step_y * 2u8).check_in(..h).ok()) {
            for x in succ(Some(offset_x), |x| (x + step_x).check_in(..w).ok()) {
                ctx.rect(*x + Self::LINE_WIDTH, *y, *step_x - Self::LINE_WIDTH, *step_y);
            }
        }
        // lighter vertical lines
        for y in succ(Some(step_y + offset_y), |y| (y + step_y * 2u8).check_in(..h).ok()) {
            for x in succ(Some(offset_x), |x| (x + step_x).check_in(..w).ok()) {
                ctx.rect(*x, *y, Self::LINE_WIDTH, *step_y);
            }
        }
        ctx.set_fill_style(&Self::MG_STYLE.into());
        ctx.fill();

        let points = Path2d::new().add_loc(loc!())?;
        let mapper = |[x, y]: [f64; 2]| [x * *step_x - self.offset.x as f64, y * *step_y - self.offset.y as f64];
        for [this, next] in self.data.array_windows::<2>() {
            points.add_path(&this.draw(Some(next), mapper).add_loc(loc!())?);
        }
        if let Some(last) = self.data.last() {
            points.add_path(&last.draw(None, mapper).add_loc(loc!())?);
        }
        ctx.fill_with_path_2d(&points);
        ctx.set_stroke_style(&Self::FG_STYLE.into());
        ctx.stroke_with_path(&points);

        if app_ctx.play_since.is_finite() {
            ctx.set_fill_style(&Self::FG_STYLE.into());
            let play_at = (app_ctx.now - app_ctx.play_since).secs_to_beats(app_ctx.bps) - app_ctx.offset;
            ctx.fill_rect(*play_at * *step_x - self.offset.x as f64, 0.0, 3.0, *h);
        } else {
            self.redraw = false;
        }

        Ok(if discriminant(&self.focus) != self.last_focus || self.focus.always_update() {
            self.last_focus = discriminant(&self.focus);
            match &self.focus {
                Focus::None => None,

                Focus::HoverPlane => Some([T::EDITOR_NAME.into(), "".into()]),

                Focus::HoverPoint(id) => {
                    let point = unsafe{self.data.get_unchecked(*id)};
                    Some([format!("{}@{}", &point.desc(), &T::fmt_loc(point.loc())).into(),
                        "Press and hold to drag".into()])
                }

                Focus::MovePlane(_) => Some([T::EDITOR_NAME.into(), "Dragging".into()]),

                Focus::MovePoint(id) => {
                    let point = unsafe{self.data.get_unchecked(*id)};
                    Some([format!("{}@{}", &point.desc(), &T::fmt_loc(point.loc())).into(),
                        "Dragging".into()])
                }

                Focus::HoverPlaneWShift(at) => {
                    let x = R64::from(at.x) / step_x;
                    let y = R64::from(at.y) / step_y;
                    ctx.set_text_align("left");
                    ctx.set_text_baseline("bottom");
                    ctx.set_fill_style(&Self::FG_STYLE.into());
                    ctx.fill_text(&T::fmt_loc([x, y]), 5.0, *h - 5.0)
                        .add_loc(loc!())?;
                    Some([T::EDITOR_NAME.into(), "Press and hold to zoom".into()])
                }

                Focus::AwaitDragIn(d) => {
                    ctx.begin_path();
                    ctx.rect(0.0, 0.0, *w, 5.0);
                    ctx.rect(0.0, 0.0, 5.0, *h);
                    ctx.rect(0.0, *h - 5.0, *w, 5.0);
                    ctx.rect(*w - 5.0, 0.0, 5.0, *h);
                    ctx.set_fill_style(&Self::FG_STYLE.into());
                    ctx.fill();
                    ctx.set_text_align("center");
                    ctx.set_text_baseline("middle");
                    ctx.fill_text("Drag here", *w / 2.0, *h / 2.0).add_loc(loc!())?;
                    T::drag_n_drop_hint(*d, || None)
                }

                Focus::HoverWDraggable(d, at, visual) => {
                    ctx.translate(at.x as f64, at.y as f64).add_loc(loc!())?;
                    ctx.stroke_with_path(visual);
                    ctx.translate(-at.x as f64, -at.y as f64).add_loc(loc!())?;
                    ctx.begin_path();
                    ctx.rect(0.0, 0.0, *w, 5.0);
                    ctx.rect(0.0, 0.0, 5.0, *h);
                    ctx.rect(0.0, *h - 5.0, *w, 5.0);
                    ctx.rect(*w - 5.0, 0.0, 5.0, *h);
                    ctx.set_fill_style(&Self::FG_STYLE.into());
                    ctx.fill();
                    T::drag_n_drop_hint(*d, || Some([R64::from(at.x) / step_x, R64::from(at.y) / step_y]))
                }

                Focus::ZoomPlane{pivot, init_offset, ..} => {
                    let [x, y] = (*pivot - *init_offset).map(|x| x as f64);
                    ctx.begin_path();
                    ctx.move_to(x - 10.0, y);
                    ctx.line_to(x + 10.0, y);
                    ctx.move_to(x, y - 10.0);
                    ctx.line_to(x, y + 10.0);
                    ctx.set_stroke_style(&Self::FG_STYLE.into());
                    ctx.stroke();
                    Some([format!("{}: zooming", T::EDITOR_NAME).into(), "Release to stop".into()])
                }
            }
        } else {None})
    }
}
