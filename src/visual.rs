use std::{
    iter::{Iterator, successors as succ, empty, once},
    slice::{from_raw_parts, from_mut, from_ref},
    mem::{replace, take},
    ops::{Range, Deref},
    fmt::Debug,
    rc::Rc,
    borrow::Cow};
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
    utils::{Check, SliceExt, Point,
        JsResult, HtmlCanvasExt, JsResultUtils, OptionExt,
        HtmlElementExt, 
        Pipe, BoolExt, RangeExt, VecExt, R64, ArrayExt, ArrayFrom, IntoArray, SliceRef, ResultToJsResult, SliceMove},
    sound::FromBeats,
    global::{AppEvent, AppContext},
    input::{Buttons, CanvasEvent},
    loc,
    r64,
    js_assert,
    eval_once,
    js_array
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
pub trait Graphable: Ord {
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
    /// type returned from `GraphEditor::handle_hover` when the user interacted with the UI in a special way,
    /// i.e. selected a point or dropped in a `Draggable`
    type Event = ();

    /// inner data of the point
    fn inner(&self) -> &Self::Inner;
    /// mutable inner data of the point
    fn inner_mut(&mut self) -> &mut Self::Inner;
    /// location of the point in user coordinates
    fn loc(&self) -> [R64; 2];
    /// change the location of the point in user coordinates when moved in the UI
    /// `meta` signifies whether the meta key was held while moving the point
    fn move_point(&mut self, delta: [R64; 2], meta: bool);
    /// returns `true` if the given user coordinates are inside the hitbox of the point
    fn in_hitbox(&self, point: [R64; 2]) -> bool;

    /// event to emit when points are moved in the UI
    /// `ids` are the current IDs of the points that were moved
    /// `meta` indicates whether the meta key was held while moving
    #[allow(unused_variables)] #[inline]
    fn on_move(ids: &[usize], n_points: usize, delta: [R64; 2], meta: bool) -> Option<Self::Event> {None} 
    /// event to emit when the left mouse button is clicked, i.e. pressed & released
    /// `loc` returns a location of the click in user coordinates.
    /// `new_sel` are the IDs of the points that became focused after the click
    #[allow(unused_variables)] #[inline]
    fn on_click<'a, F, I1, I2>(loc: F, old_sel: I1, new_sel: I2, meta: bool) -> Option<Self::Event>
    where Self: 'a,
    F: Fn() -> [R64; 2],
    I1: Iterator<Item=SliceRef<'a, Self>> + ExactSizeIterator,
    I2: Iterator<Item=SliceRef<'a, Self>> + ExactSizeIterator {
        None
    }

    // return type of `desc` and `fmt_loc` should be `impl AsRef<str>`
    // but feature `return_position_impl_trait_in_trait` is not usable yet
    /// description of the point that'll be shown in the hint handler when the point's hovered over
    #[allow(unused_variables)] #[inline]
    fn desc(&self) -> Cow<'static, str> {Cow::default()}
    /// format the location of a point in user coordinates
    /// `loc` is in user coordinates
    fn fmt_loc(loc: [R64; 2]) -> String;
    /// The hint to show when the plane is being hovered.
    /// `loc` is in user coordinates.
    fn plane_hover_hint(loc: impl Fn() -> [R64; 2], buttons: Buttons) -> Option<[Cow<'static, str>; 2]>;
    /// The hint to show when a point is being hovered
    fn point_hover_hint<'a>(point: SliceRef<'a, Self>, buttons: Buttons) -> Option<[Cow<'static, str>; 2]>
    where Self: 'a;
    /// The hint to show when a selection is being hovered
    fn selection_hover_hint<'a, I>(points: I, buttons: Buttons) -> Option<[Cow<'static, str>; 2]>
    where Self: 'a, I: Iterator<Item=SliceRef<'a, Self>> + ExactSizeIterator;

    /// visual representation of the point's hitbox
    /// `mapper` maps user coordinates to canvas coordinates, all vertices in the
    /// returned `Path2d` must be the result of the `mapper` function
    fn draw(&self, next: Option<&Self>, mapper: impl Fn([R64; 2]) -> [R64; 2]) -> JsResult<Path2d>;
    /// visual representation of the meta key and left mouse button being held, i.e. of a selected
    /// region
    /// `origin` and `cur` are in canvas coordinates
    #[allow(unused_variables)] #[inline]
    fn draw_meta_drag(canvas_size: [R64; 2], scale: [R64; 2], origin: [R64; 2], cur: [R64; 2]) -> JsResult<Path2d> {
        Path2d::new().add_loc(loc!()).inspect(|x|
            x.rect(*cur[0], *cur[1], *origin[0] - *cur[0], *origin[1] - *cur[1]))
    }
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

// coordinate space hints:
// +user: in user coordinates
// +canvas: in canvas coordinates without offset
// +aligned: aligned to snap step
// +confined: limited to bounds specified by `Graphable::(X/Y)_BOUND`

#[derive(Debug, Clone, PartialEq, Eq, Default)]
enum Focus {
    #[default] None,
    Zoom{init_offset: Point, pivot: Point, init_scale: [R64; 2]},
    Plane{last_loc: Point /* +canvas +aligned */},
    Point{id: usize, last_loc: [R64; 2] /* +user +aligned +confined */},
    Selection{ids: Box<[usize]> /* sorted */, last_loc: [R64; 2] /* +user +aligned +confined */, moved: bool}
}

impl Focus {
    #[inline] pub fn selection(&self) -> &[usize] {
        if let Focus::Selection{ids, ..} = self {ids} else {&[]}
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GraphEditor<T: Graphable> {
    canvas: NodeRef,
    offset: Point,
    scale: [R64; 2],
    focus: Focus,
    last_event: CanvasEvent,
    redraw: bool,
    data: Vec<T>
}

impl<T: Graphable> GraphEditor<T> {
    const FONT: &str = "20px consolas";
    const BG_STYLE: &str = "#232328";
    const MG_STYLE: &str = "#333338";
    const FG_STYLE: &str = "#0069E1";
    const LINE_WIDTH: f64 = 3.0;

    #[inline] pub fn new(data: Vec<T>) -> Self {
        Self{canvas: NodeRef::default(), offset: Point::ZERO,
            focus: Focus::None, last_event: CanvasEvent::default(),
            scale: [T::SCALE_X_BOUND.to_pair().mul(&[0.75, 0.25]).sum(), T::SCALE_Y_BOUND.to_pair().mul(&[0.75, 0.25]).sum()],
            redraw: false, data}
    }

    #[inline] pub fn canvas(&self) -> &NodeRef {&self.canvas}

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

    #[inline] pub fn selection(&self) -> &[usize] {self.focus.selection()}

    #[inline] pub fn iter_selection(&self) -> impl Iterator<Item=&'_ T> {
        self.focus.selection().iter().map(|x| unsafe{self.data.get_unchecked(*x)})
    }

    /// returns the index at which the new point will be available
    #[inline] pub fn add_point(&mut self, point: T) -> usize {
        self.redraw = true;
        let to = self.data.push_sorted(point);
        if let Focus::Selection{ids, ..} = &mut self.focus {
            SliceMove{from: self.data.len(), to}.apply(ids)
        }
        to
    }

    #[inline] pub fn remove_points(&mut self, to_remove: &[usize]) -> JsResult<()> {
        self.redraw = true;
        js_assert!(to_remove.is_sorted())?;
        if let Focus::Selection{ids, ..} = &mut self.focus {
            let mut ids = take(ids).to_vec();
            let mut ids_iter = ids.iter_mut().rev();
            for &id in to_remove.iter().rev() {
                self.data.try_remove(id).to_js_result(loc!())?;
                let rem = ids_iter.len();
                while let Some(x) = ids_iter.next().filter(|x| **x > id) {*x -= 1}
                let skipped = rem - ids_iter.len();
                if ids_iter.next().copied() == Some(id) {
                    unsafe{ids_iter.len().pipe(|x| ids.remove_unchecked(x))};
                }
                ids_iter = ids.iter_mut().rev();
                ids_iter.advance_by(skipped).to_js_result(loc!())?;
            }
        } else {
            for &id in to_remove.iter().rev() {
                self.data.try_remove(id).to_js_result(loc!())?;
            }
        }
        Ok(())
    }

    #[inline] pub fn force_redraw(&mut self) {self.redraw = true}

    /// must be called when a canvas has just been bound or its dimensions have been changed
    pub fn init(&mut self, resizer: impl FnOnce(&HtmlCanvasElement) -> JsResult<[u32; 2]>) -> JsResult<()> {
        let canvas: HtmlCanvasElement = self.canvas.cast().to_js_result(loc!())?;
        let [w, h] = resizer(&canvas).add_loc(loc!())?;
        canvas.set_width(w);
        canvas.set_height(h);
        if self.offset.x <= 0 {
            self.offset.x = (T::OFFSET_X_BOUND.start * R64::from(w) / self.scale[0]).into();
        }
        if self.offset.y <= 0 {
            self.offset.y = (T::OFFSET_Y_BOUND.start * R64::from(h) / self.scale[1]).into();
        }
        let ctx = canvas.get_2d_context(loc!())?;
        ctx.set_font(Self::FONT);
        ctx.set_line_width(Self::LINE_WIDTH);
        Ok(self.redraw = true)
    }

    #[inline] fn point_by_pos(&self, loc: [R64; 2]) -> Option<SliceRef<'_, T>> {
        self.data.iter().enumerate().find(|(_, x)| x.in_hitbox(loc))
            .map(|(id, x)| unsafe{SliceRef::raw(x, id)})
    }

    #[inline] fn set_focus(&mut self, focus: Focus) {
        self.focus = focus;
        self.redraw = true;
    }

    pub fn handle_hover(&mut self, event: Option<CanvasEvent>, ctx: &AppContext) -> JsResult<Option<T::Event>> {
        let Some(event) = event else {
            self.focus = Focus::None;
            self.redraw = true;
            return Ok(None)
        };

        let size   = self.canvas.cast::<HtmlCanvasElement>()
            .to_js_result(loc!())?.size();
        let snap_step = &[ctx.snap_step, T::Y_SNAP];
        let step     = &R64::array_from(size).div(&self.scale);

        // let canvas_plane = event.point;
        let offset = |loc| loc + self.offset;
        let to_user = |loc| R64::array_from(loc + self.offset).div(step);
        let to_aligned_canvas = |loc| Point::from(R64::array_from(loc).floor_to(&snap_step.mul(step)));
        let to_aligned_user = |loc| R64::array_from(loc + self.offset).div(step).floor_to(snap_step);
        let confine = |x| [T::X_BOUND, T::Y_BOUND].fit(x);

        let zoom_focus = ||
            Focus::Zoom{init_offset: self.offset, pivot: offset(event.point), init_scale: self.scale};
        let plane_focus = ||
            Focus::Plane{last_loc: to_aligned_canvas(event.point)};
        let point_focus = |id|
            Focus::Point{id, last_loc: confine(to_aligned_user(event.point))};
        let selection_focus = |ids|
            Focus::Selection{ids, last_loc: confine(to_aligned_user(event.point)), moved: false};

        let res = Ok(match self.focus {
            Focus::None => self.set_focus(plane_focus()).pipe(|_| None),

            Focus::Zoom{ref mut init_offset, ref mut init_scale, ref mut pivot} => if event.left {
                if !self.last_event.left {
                    *pivot = offset(event.point);
                    *init_scale = self.scale;
                    *init_offset = self.offset;
                }
                let point = event.point + *init_offset - *pivot;
                if !T::SCALE_X_BOUND.is_empty() {
                    self.scale[0] = T::SCALE_X_BOUND.fit(r64![50.0] / size[1] * point.x + init_scale[0]);
                    self.offset.x = ((init_scale[0] - self.scale[0]) * pivot.x / self.scale[0] + init_offset.x).into();
                }
                if !T::SCALE_Y_BOUND.is_empty() {
                    self.scale[1] = T::SCALE_Y_BOUND.fit(r64![50.0] / size[0] * point.y + init_scale[1]);
                    self.offset.y = ((init_scale[1] - self.scale[1]) * pivot.y / self.scale[1] + init_offset.y).into();
                }
            } else if event.shift {
                *pivot = event.point;
            } else {
                self.focus = plane_focus();
            }.pipe(|_| {self.redraw = true; None}),

            Focus::Plane{ref mut last_loc} => match *event {
                Buttons{shift: true, ..} => self.set_focus(zoom_focus()).pipe(|_| None),

                Buttons{left: false, meta, ..} => if self.last_event.left && self.last_event.meta {
                    let src = to_user(*last_loc);
                    let dst = to_user(event.point);
                    let area = [(src[0] .. dst[0]).ordered(), (src[1] .. dst[1]).ordered()];
                    let sel: Box<_> = self.data.iter().enumerate()
                        .filter_map(|(id, x)| x.loc().array_check_in(&area).map(|_| id))
                        .collect();
                    let res = T::on_click(|| confine(to_aligned_user(event.point)),
                        empty(),
                        sel.iter().map(|i| unsafe{self.data.get_unchecked_aware(*i)}),
                        meta);
                    if !sel.is_empty() {
                        self.set_focus(selection_focus(sel))
                    } else if meta {
                        *last_loc = to_aligned_canvas(event.point);
                        self.redraw = true;
                    }
                    res
                } else if let Some(p) = self.point_by_pos(to_user(event.point)) {
                    self.set_focus(point_focus(p.index()));
                    None
                } else if meta {
                    self.set_focus(plane_focus());
                    None
                } else {None}

                Buttons{left: true, meta: false, ..} => {
                    if !T::OFFSET_X_BOUND.is_empty() {
                        self.offset.x = T::OFFSET_X_BOUND.map(|x| x * step[0])
                            .extend(self.offset.x).fit(self.offset.x + last_loc.x - event.point.x);
                    }
                    if !T::OFFSET_Y_BOUND.is_empty() {
                        self.offset.y = T::OFFSET_Y_BOUND.map(|y| y * step[1])
                            .extend(self.offset.y).fit(self.offset.y + last_loc.y - event.point.y);
                    }
                    *last_loc = event.point;
                    self.redraw = true;
                    None
                }

                Buttons{left: true, meta: true, ..} => {
                    if !self.last_event.left {
                        *last_loc = to_aligned_canvas(event.point);
                    }
                    self.redraw = true;
                    None
                }
            }

            Focus::Point{ref mut id, ref mut last_loc} => if event.shift {
                self.set_focus(zoom_focus());
                None
            } else if event.left {
                let delta = if !self.last_event.left {
                    *last_loc = confine(to_aligned_user(event.point));
                    Default::default()
                } else {
                    let new = confine(to_aligned_user(event.point));
                    new.sub(&replace(last_loc, new))
                };
                unsafe{self.data.get_unchecked_mut(*id)}.move_point(delta, event.meta);
                unsafe{self.data.reorder_unchecked(*id)}.apply(from_mut(id));
                T::on_move(from_ref(id), self.data.len(), delta, event.meta)
            } else {
                let p = unsafe{self.data.get_unchecked_aware(*id)};
                let (f, r) = match (self.last_event.left, p.in_hitbox(to_user(event.point))) {
                    (false, false) =>
                        (Some(plane_focus()), None),
                    (false, true) =>
                        (None, None),
                    (true, false) => (Some(plane_focus()),
                        T::on_click(|| confine(to_aligned_user(event.point)),
                            empty(), empty(), self.last_event.meta)),
                    (true, true) => (Some(selection_focus([*id].into())),
                        T::on_click(|| confine(to_aligned_user(event.point)),
                            empty(), once(p), self.last_event.meta))
                };
                if let Some(f) = f {self.set_focus(f)}
                r
            }

            Focus::Selection{ref mut ids, ref mut last_loc, ref mut moved} => if event.shift {
                self.set_focus(zoom_focus());
                None
            } else if event.left {
                let delta = if !self.last_event.left {
                    *last_loc = confine(to_aligned_user(event.point));
                    self.redraw = true;
                    Default::default()
                } else {
                    let new = confine(to_aligned_user(event.point));
                    new.sub(&replace(last_loc, new))
                };
                if *delta.sum::<R64>() != 0.0 {
                    *moved = true;
                    self.redraw = true;
                    // TODO: some optimisation
                    for &id in ids.clone().iter() {
                        unsafe{self.data.get_unchecked_mut(id)}.move_point(delta, event.meta);
                        unsafe{self.data.reorder_unchecked(id)}.apply(ids);
                    }
                    T::on_move(ids, self.data.len(), delta, event.meta)
                } else {None}
            } else if self.last_event.left && !*moved {
                let e = T::on_click(|| confine(to_aligned_user(event.point)),
                    ids.iter().map(|i| unsafe{self.data.get_unchecked_aware(*i)}),
                    empty(),
                    self.last_event.meta);
                self.set_focus(self.point_by_pos(to_user(event.point))
                    .map_or_else(plane_focus, |p| point_focus(p.index())));
                e
            } else {
                *moved = false;
                None
            }
        });
        self.redraw |= *self.last_event != *event;
        self.last_event = event;
        res
    }

    pub fn redraw(&mut self, app_ctx: &AppContext) -> JsResult<Option<[Cow<'static, str>; 2]>> {
        if !self.redraw {return Ok(None)}

        let canvas: HtmlCanvasElement = self.canvas().cast().to_js_result(loc!())?;
        let size = canvas.size().map(R64::from);
        let snap_step = &[app_ctx.snap_step, T::Y_SNAP];
        let step     = &R64::array_from(size).div(&self.scale);
        let ctx = canvas.get_2d_context(loc!())?;

        let to_user = |loc| R64::array_from(loc + self.offset).div(step);
        let to_aligned_user = |loc| R64::array_from(loc + self.offset).div(step).floor_to(snap_step);
        let confine = |x| [T::X_BOUND, T::Y_BOUND].fit(x);

        ctx.set_fill_style(&Self::BG_STYLE.into());
        ctx.fill_rect(0.0, 0.0, *size[0], *size[1]);

        let step = &size.div(&self.scale);
        let offset_x = R64::from(-self.offset.x)
            % (self.offset.x > (T::X_BOUND.start * step[0]).into())
                .choose(step[0], R64::INFINITY);
        let offset_y = R64::from(-self.offset.y)
            % (self.offset.y > (T::Y_BOUND.start * step[1]).into())
            .choose(step[1] * 2.0, R64::INFINITY);
        let max_x = size[0].min(T::X_BOUND.end * step[0] - self.offset.x);
        let max_y = size[1].min(T::Y_BOUND.end * step[1] - self.offset.y);

        // lighter horizontal bars
        ctx.begin_path();
        for y in succ(Some(offset_y), |y| (y + step[1] * 2u8).check_in(..max_y).ok()) {
            for x in succ(Some(offset_x), |x| (x + step[0]).check_in(..max_x).ok()) {
                ctx.rect(*x + Self::LINE_WIDTH, *y, *step[0] - Self::LINE_WIDTH, *step[1]);
            }
        }
        // lighter vertical lines
        for y in succ(Some(step[1] + offset_y), |y| (y + step[1] * 2u8).check_in(..max_y).ok()) {
            for x in succ(Some(offset_x), |x| (x + step[0]).check_in(..max_x).ok()) {
                ctx.rect(*x, *y, Self::LINE_WIDTH, *step[1]);
            }
        }
        ctx.set_fill_style(&Self::MG_STYLE.into());
        ctx.fill();

        let points = Path2d::new().add_loc(loc!())?;
        let mapper = |loc: [R64; 2]| loc.mul(step).sub(&R64::array_from(self.offset));
        ctx.set_stroke_style(&Self::FG_STYLE.into());
        if let Ok(ids) = self.focus.selection().check(|x| !x.is_empty()) {
            let non_ids = ids.first().map_or(0..0, |x| 0..*x)
                .chain(ids.array_windows::<2>().flat_map(|[p, n]| p + 1 .. *n))
                .chain(ids.last().map_or(0..0, |x| x + 1 .. self.data.len()));

            for (this, next) in non_ids.map(|i| (unsafe{self.get_unchecked(i)}, self.get(i + 1))) {
                points.add_path(&this.draw(next, mapper).add_loc(loc!())?)
            }
            ctx.fill_with_path_2d(&points);
            ctx.set_line_dash(eval_once!(JsValue: js_array![number 10.0, number 10.0])).add_loc(loc!())?;
            ctx.stroke_with_path(&points);
            ctx.set_line_dash(eval_once!(JsValue: js_array![])).add_loc(loc!())?;

            let points = Path2d::new().add_loc(loc!())?;
            for (this, next) in ids.iter().map(|i| (unsafe{self.get_unchecked(*i)}, self.get(i + 1))) {
                points.add_path(&this.draw(next, mapper).add_loc(loc!())?)
            }
            ctx.fill_with_path_2d(&points);
            ctx.stroke_with_path(&points);

        } else {
            for [this, next] in self.data.array_windows::<2>() {
                points.add_path(&this.draw(Some(next), mapper).add_loc(loc!())?);
            }
            if let Some(last) = self.data.last() {
                points.add_path(&last.draw(None, mapper).add_loc(loc!())?);
            }
            ctx.fill_with_path_2d(&points);
            ctx.stroke_with_path(&points);
        }

        if app_ctx.play_since.is_finite() {
            ctx.set_fill_style(&Self::FG_STYLE.into());
            let play_at = (app_ctx.now - app_ctx.play_since).secs_to_beats(app_ctx.bps);
            ctx.fill_rect(*play_at * *step[0] - self.offset.x as f64, 0.0, 3.0, *size[1]);
        } else {
            self.redraw = false;
        }

        Ok(match self.focus {
            Focus::None => None,

            Focus::Zoom{pivot, init_offset, ..} => if self.last_event.left {
                let [x, y] = (pivot - init_offset).map(|x| x as f64);
                ctx.begin_path();
                ctx.move_to(x - 10.0, y);
                ctx.line_to(x + 10.0, y);
                ctx.move_to(x, y - 10.0);
                ctx.line_to(x, y + 10.0);
                ctx.set_stroke_style(&Self::FG_STYLE.into());
                ctx.stroke();
                Some([Cow::from(T::EDITOR_NAME) + ": zooming", "Release to stop".into()])
            } else {
                ctx.set_text_align("left");
                ctx.set_text_baseline("bottom");
                ctx.set_fill_style(&Self::FG_STYLE.into());
                ctx.fill_text(&T::fmt_loc(confine(to_user(pivot))), 5.0, *size[1] - 5.0)
                    .add_loc(loc!())?;
                Some([T::EDITOR_NAME.into(), "Press and hold to zoom".into()])
            }

            Focus::Plane{last_loc} => if self.last_event.meta {
                if self.last_event.left {
                    let [cur, origin] = [self.last_event.point.into_array(), last_loc.into_array()];
                    ctx.set_line_dash(eval_once!(JsValue: js_array![number 10.0, number 10.0])).add_loc(loc!())?;
                    ctx.stroke_rect(cur[0], cur[1], origin[0] - cur[0], origin[1] - cur[1]);
                    ctx.set_line_dash(eval_once!(JsValue: js_array![])).add_loc(loc!())?;
                } else {
                    let [x, y] = last_loc.map(|x| x as f64);
                    ctx.set_line_dash(eval_once!(JsValue: js_array![number 10.0, number 10.0])).add_loc(loc!())?;
                    ctx.move_to(-*size[0], y);
                    ctx.line_to( *size[0], y);
                    ctx.move_to(x, -*size[1]);
                    ctx.line_to(x,  *size[1]);
                    ctx.set_line_dash(eval_once!(JsValue: js_array![])).add_loc(loc!())?;
                }
            }.pipe(|_| T::plane_hover_hint(|| confine(to_aligned_user(self.last_event.point)), *self.last_event)),

            Focus::Point{id, ..} =>
                T::point_hover_hint(unsafe{self.get_unchecked_aware(id)}, *self.last_event),

            Focus::Selection{ref ids, ..} =>
                T::selection_hover_hint(ids.iter().map(|x| unsafe{self.get_unchecked_aware(*x)}), *self.last_event)
        })
    }
}
