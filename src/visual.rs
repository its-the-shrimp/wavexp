use std::{
    iter::{Iterator, empty, once},
    slice::{from_raw_parts, from_mut, from_ref},
    mem::{replace, take},
    ops::{Range, Deref, Not},
    fmt::Debug,
    rc::Rc,
    borrow::Cow,
    cmp::{min, Ordering},
    cell::LazyCell};
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
        Pipe, BoolExt, RangeExt, VecExt, R64, ArrayExt, ArrayFrom, IntoArray, SliceRef, ResultToJsResult, SliceMove, RoundTo, FlippedArray, default},
    sound::{FromBeats, Secs},
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
    fn move_point(point: Result<&mut Self, &mut [R64; 2]>, delta: [R64; 2], meta: bool);
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
    #[allow(unused_variables)] #[inline] fn on_click<'a>(
        pressed_at:    impl Deref<Target=[R64; 2]>,
        released_at:   impl Deref<Target=[R64; 2]>,
        old_selection: impl Iterator<Item=SliceRef<'a, Self>> + ExactSizeIterator,
        new_selection: impl Iterator<Item=SliceRef<'a, Self>> + ExactSizeIterator,
        meta: bool)
    -> Option<Self::Event> where Self: 'a {
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
    /// the canvas's coordinate space
    /// the function will be called every time the user 
    #[inline] fn canvas_coords(canvas: &HtmlCanvasElement) -> JsResult<[u32; 2]> {
        Ok([canvas.client_width() as u32, canvas.client_height() as u32])
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

// types as coordinate space hints:
/// in canvas coordinates with plane offset
type OffsetCanvasPoint = Point;
/// in user coordinates, aligned to snap step and limited to bounds specified by `Graphable::(X/Y)_BOUND`
type ConfinedAlignedUserPoint = [R64; 2];

#[derive(Default, Debug, Clone, PartialEq, Eq)]
enum Focus {
    #[default] None,
    Zoom{init_offset: Point, pivot: OffsetCanvasPoint, init_scale: [R64; 2]},
    Plane{origin: ConfinedAlignedUserPoint, press_time: Secs},
    Point{id: usize, last_loc: ConfinedAlignedUserPoint, origin: ConfinedAlignedUserPoint},
    Selection{origin: ConfinedAlignedUserPoint, end: ConfinedAlignedUserPoint}
}

#[derive(Debug, Clone, PartialEq)]
pub struct GraphEditor<T: Graphable> {
    canvas: NodeRef,
    offset: Point,
    scale: [R64; 2],
    focus: Focus,
    selection: Vec<usize>,
    selection_origin: ConfinedAlignedUserPoint,
    selection_size: [R64; 2],
    last_event: CanvasEvent,
    redraw: bool,
    data: Vec<T>,
    grid: Option<(Path2d, [R64; 2])>
}

impl<T: Graphable> GraphEditor<T> {
    const FONT: &str = "20px consolas";
    const BG_STYLE: &str = "#232328";
    const MG_STYLE: &str = "#333338";
    const FG_STYLE: &str = "#0069E1";
    const LINE_WIDTH: f64 = 3.0;

    #[inline] pub fn new(data: Vec<T>) -> Self {
        Self{canvas: default(),
            offset: Point::ZERO,
            focus: default(),
            selection: vec![],
            selection_origin: default(),
            selection_size: default(),
            last_event: default(),
            scale: [T::SCALE_X_BOUND, T::SCALE_Y_BOUND]
                .map(|x| x.to_pair().mul(&[0.75, 0.25]).sum()),
            redraw: false,
            grid: None,
            data}
    }

    #[inline] pub fn last_event(&self) -> Option<CanvasEvent> {
        matches!(self.focus, Focus::None{..}).not().then_some(self.last_event)
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

    #[inline] pub fn selection(&self) -> &[usize] {&self.selection}

    /// returns the index at which the new point will be available
    #[inline] pub fn add_point(&mut self, point: T) -> usize {
        self.redraw = true;
        let to = self.data.push_sorted(point);
        SliceMove{from: self.data.len(), to}.apply(&mut self.selection);
        to
    }

    #[inline] pub fn remove_points(&mut self, to_remove: &[usize]) -> JsResult<()> {
        self.redraw = true;
        js_assert!(to_remove.is_sorted())?;
        let mut ids_iter = self.selection.iter_mut().rev();
        for &id in to_remove.iter().rev() {
            self.data.try_remove(id).to_js_result(loc!())?;
            let rem = loop {
                let Some(x) = ids_iter.next() else {break 0};
                match id.cmp(x) {
                    Ordering::Less =>
                        *x -= 1,
                    Ordering::Equal =>
                        break ids_iter.len().pipe(|x| unsafe{self.selection.remove_unchecked(x); x}),
                    Ordering::Greater =>
                        break ids_iter.len()
                }
            };
            ids_iter = self.selection.get_mut(..rem).unwrap_or(&mut []).iter_mut().rev();
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
        ctx.set_font(Self::FONT);
        ctx.set_line_width(Self::LINE_WIDTH);
        Ok(self.redraw = true)
    }

    #[inline] fn point_by_pos(&self, loc: [R64; 2]) -> Option<SliceRef<'_, T>> {
        self.data.iter().enumerate().find(|(_, x)| x.in_hitbox(loc))
            .map(|(id, x)| unsafe{SliceRef::raw(x, id)})
    }

    #[inline] fn point_in_selection(&self, loc: ConfinedAlignedUserPoint) -> bool {
        loc.sub(&self.selection_origin).zip_fold(true, self.selection_size, |r, x, y| r && 0.0 <= *x && x <= y)
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
        let snap_step = [ctx.snap_step, T::Y_SNAP];
        let step     = &R64::array_from(size).div(&self.scale);

        let offset = |loc| loc + self.offset;
        let to_user = |loc| R64::array_from(loc + self.offset).div(step);
        let user_align = |loc: [R64; 2]| loc.floor_to(snap_step);
        let to_aligned_user = |loc| R64::array_from(loc + self.offset).div(step).floor_to(snap_step);
        let confine = |x| [T::X_BOUND, T::Y_BOUND].fit(x);

        let zoom_focus = ||
            Focus::Zoom{init_offset: self.offset, pivot: offset(event.point), init_scale: self.scale};
        let plane_focus = ||
            Focus::Plane{origin: default(), press_time: default()};
        let point_focus = |id|
            Focus::Point{id, last_loc: default(), origin: default()};
        let selection_focus = ||
            Focus::Selection{origin: default(), end: default()};

        let res = Ok(match &mut self.focus {
            Focus::None => self.set_focus(plane_focus()).pipe(|_| None),

            Focus::Zoom{init_offset, init_scale, pivot} => if event.left {
                if !self.last_event.left {
                    *pivot = offset(event.point);
                    *init_scale = self.scale;
                    *init_offset = self.offset;
                } else {
                    self.redraw = true;
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
                self.redraw = true;
            } else {
                self.focus = plane_focus();
            }.pipe(|_| None),

            Focus::Plane{origin, press_time} => match *event {
                Buttons{shift: true, ..} => self.set_focus(zoom_focus()).pipe(|_| None),

                Buttons{left: false, meta, ..} => if self.last_event.left {
                    let cur_loc = to_user(event.point);
                    if self.last_event.meta {
                        let end = confine(user_align(cur_loc));
                        let old_selection = self.selection.clone();
                        let area = [*origin, end].flipped().map(|x| (x[0] .. x[1]).ordered());
                        self.selection_origin = area.clone().map(|x| x.start);
                        self.selection_size = area.clone().map(|x| x.end - x.start);
                        self.selection = self.data.iter().enumerate()
                            .filter_map(|(id, x)| x.loc().array_check_in(&area).map(|_| id))
                            .collect();
                        let res = T::on_click(origin, &end,
                            old_selection.iter().map(|i| unsafe{self.data.get_unchecked_aware(*i)}),
                            self.selection.iter().map(|i| unsafe{self.data.get_unchecked_aware(*i)}),
                            meta);
                        self.focus = self.point_by_pos(to_user(event.point))
                            .map_or_else(plane_focus, |p| point_focus(p.index()));
                        res
                    } else {
                        let cur_loc = confine(user_align(cur_loc));
                        let ids_iter = self.selection.iter().map(|i| unsafe{self.data.get_unchecked_aware(*i)});
                        if *ctx.now - **press_time > 0.33 {
                            T::on_click(&cur_loc, &cur_loc,
                                ids_iter.clone(), ids_iter,
                                meta)
                        } else {
                            let res = T::on_click(&cur_loc, &cur_loc,
                                ids_iter, empty(),
                                meta);
                            self.selection.clear();
                            self.selection_size = default();
                            res
                        }
                    }
                } else {
                    let loc = to_user(event.point);
                    if self.point_in_selection(loc) {
                        self.set_focus(selection_focus());
                    } else if let Some(p) = self.point_by_pos(loc) {
                        self.set_focus(point_focus(p.index()));
                    } else {
                        self.redraw |= self.last_event.meta | meta;
                    }
                    None
                }

                Buttons{left: true, meta, ..} => if meta {
                    if !self.last_event.left {
                        *origin = confine(to_aligned_user(event.point));
                    } else {self.redraw = true}
                } else {
                    if !self.last_event.left {
                        *press_time = ctx.now;
                    } else {self.redraw = true}

                    if !T::OFFSET_X_BOUND.is_empty() {
                        self.offset.x = T::OFFSET_X_BOUND.map(|x| x * step[0])
                            .extend(self.offset.x).fit(self.offset.x + self.last_event.point.x - event.point.x);
                    }
                    if !T::OFFSET_Y_BOUND.is_empty() {
                        self.offset.y = T::OFFSET_Y_BOUND.map(|y| y * step[1])
                            .extend(self.offset.y).fit(self.offset.y + self.last_event.point.y - event.point.y);
                    }
                }.pipe(|_| None)
            }

            Focus::Point{id, last_loc, origin} => if event.shift {
                self.focus = zoom_focus();
                None
            } else if event.left {
                let delta = if !self.last_event.left {
                    *last_loc = confine(to_aligned_user(event.point));
                    *origin = *last_loc;
                    Default::default()
                } else {
                    let new = confine(to_aligned_user(event.point));
                    new.sub(&replace(last_loc, new))
                };
                if *delta.sum::<R64>() != 0.0 {
                    self.redraw = true;
                    T::move_point(Ok(unsafe{self.data.get_unchecked_mut(*id)}), delta, event.meta);
                    unsafe{self.data.reorder_unchecked(*id)}.apply(from_mut(id));
                    T::on_move(from_ref(id), self.data.len(), delta, event.meta)
                } else {None}
            } else {
                let p = unsafe{self.data.get_unchecked_aware(*id)};
                let loc = to_user(event.point);
                let (f, r) = match (self.last_event.left, p.in_hitbox(loc)) {
                    (false, false) =>
                        (Some(plane_focus()), None),
                    (false, true) =>
                        (None, None),
                    (true, false) => (Some(plane_focus()),
                        T::on_click(origin,
                            LazyCell::new(|| confine(user_align(loc))),
                            empty(), empty(),
                            self.last_event.meta)),
                    (true, true) => {
                        [*id][..].clone_into(&mut self.selection);
                        self.selection_size = default();
                        let dst = confine(user_align(loc));
                        (Some(selection_focus()),
                            T::on_click(origin, &dst,
                                empty(), once(p),
                                self.last_event.meta))
                    }
                };
                if let Some(f) = f {self.set_focus(f)}
                r
            }

            Focus::Selection{origin, end} => if event.shift {
                self.focus = zoom_focus();
                None
            } else if event.left {
                let delta = if !self.last_event.left {
                    *end = confine(to_aligned_user(event.point));
                    *origin = *end;
                    Default::default()
                } else {
                    let new = confine(to_aligned_user(event.point));
                    new.sub(&replace(end, new))
                };
                if *delta.sum::<R64>() != 0.0 {
                    self.redraw = true;
                    for (ids, id) in self.selection.iter_mut_with_ctx() {
                        T::move_point(Ok(unsafe{self.data.get_unchecked_mut(id)}), delta, event.meta);
                        unsafe{self.data.reorder_unchecked(id)}.apply(ids);
                    }
                    T::move_point(Err(&mut self.selection_origin), delta, event.meta);
                    self.selection.sort_unstable();
                    T::on_move(&self.selection, self.data.len(), delta, event.meta)
                } else {None}
            } else if self.last_event.left {
                let cur_loc = confine(to_aligned_user(event.point));
                let iter = self.selection.iter().map(|i| unsafe{self.data.get_unchecked_aware(*i)});
                let origin = take(origin);
                *end = default();
                T::on_click(&origin, &cur_loc,
                    iter.clone(), iter,
                    self.last_event.meta)
            } else {
                let loc = to_user(event.point);
                if !self.point_in_selection(loc) {
                    self.set_focus(self.point_by_pos(loc).map_or_else(plane_focus, |p| point_focus(p.index())));
                }
                None
            }
        });
        self.redraw |= *event != *replace(&mut self.last_event, event);
        res
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
                res.rect(*x + Self::LINE_WIDTH, *y, *step[0] - Self::LINE_WIDTH, *step[1]);
                res.rect(*x, *y + *step[1], Self::LINE_WIDTH, *step[1]);
            }
        }

        Ok((res, scale))
    }

    pub fn redraw(&mut self, app_ctx: &AppContext) -> JsResult<Option<[Cow<'static, str>; 2]>> {
        if !self.redraw {return Ok(None)}

        let canvas: HtmlCanvasElement = self.canvas().cast().to_js_result(loc!())?;
        let size = canvas.size().map(R64::from);
        let snap_step = [app_ctx.snap_step, T::Y_SNAP];
        let ctx = canvas.get_2d_context(loc!())?;

        let step = &size.div(&self.scale);
        let offset_x = R64::from(-self.offset.x)
            % (self.offset.x > (T::X_BOUND.start * step[0]).into())
                .choose(step[0], R64::INFINITY);
        let offset_y = R64::from(-self.offset.y)
            % (self.offset.y > (T::Y_BOUND.start * step[1]).into())
            .choose(step[1] * 2.0, R64::INFINITY);

        let offset = |loc| loc + self.offset;
        let to_user = |loc| R64::array_from(offset(loc)).div(step);
        let to_aligned_canvas = |loc: Point| loc.floor_to(snap_step.mul(step).into());
        let to_aligned_user = |loc| R64::array_from(loc + self.offset).div(step).floor_to(snap_step);
        let confine = |x| [T::X_BOUND, T::Y_BOUND].fit(x);

        ctx.set_fill_style(&Self::BG_STYLE.into());
        ctx.fill_rect(0.0, 0.0, *size[0], *size[1]);

        let (grid, original_scale) = self.grid.get_or_try_insert(||
            Self::draw_grid(size, *step, self.scale).add_loc(loc!()))?;
        let grid_scale = original_scale.div(&self.scale);
        let repetitions = self.scale.sub(&[offset_x, offset_y].div(step))
            .div(original_scale)
            .map(|x| usize::from(x.ceil()));

        ctx.set_fill_style(&Self::MG_STYLE.into());
        ctx.transform(*grid_scale[0], 0.0, 0.0, *grid_scale[1], *offset_x, *offset_y).add_loc(loc!())?;

        for _ in 0 .. repetitions[1] {
            for _ in 0 .. repetitions[0] {
                ctx.fill_with_path_2d(grid);
                ctx.translate(*size[0], 0.0).add_loc(loc!())?;
            }
            ctx.translate(-*size[0] * repetitions[0] as f64, *size[1]).add_loc(loc!())?;
        }
        ctx.reset_transform().add_loc(loc!())?;

        let points = Path2d::new().add_loc(loc!())?;
        let mapper = |loc: [R64; 2]| loc.mul(step).sub(&R64::array_from(self.offset));
        for [this, next] in self.data.array_windows::<2>() {
            points.add_path(&this.draw(Some(next), mapper).add_loc(loc!())?);
        }
        if let Some(last) = self.data.last() {
            points.add_path(&last.draw(None, mapper).add_loc(loc!())?);
        }
        ctx.fill_with_path_2d(&points);
        ctx.set_stroke_style(&Self::FG_STYLE.into());
        ctx.stroke_with_path(&points);

        let [x, y] = mapper(self.selection_origin);
        let [w, h] = self.selection_size.mul(step);
        ctx.set_line_dash(eval_once!(JsValue: js_array![number 10.0, number 10.0])).add_loc(loc!())?;
        ctx.stroke_rect(*x, *y, *w, *h);
        ctx.set_line_dash(eval_once!(JsValue: js_array![])).add_loc(loc!())?;

        if app_ctx.play_since.is_finite() {
            ctx.set_fill_style(&Self::FG_STYLE.into());
            let play_at = (app_ctx.now - app_ctx.play_since).secs_to_beats(app_ctx.bps);
            ctx.fill_rect(*play_at * *step[0] - self.offset.x as f64, 0.0, 3.0, *size[1]);
        } else {
            self.redraw = false;
        }

        Ok(match self.focus {
            Focus::None{..} => None,

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

            Focus::Plane{origin, ..} => if self.last_event.meta {
                if self.last_event.left {
                    let cur = to_aligned_canvas(self.last_event.point);
                    let origin = mapper(origin).map(|x| *x);
                    ctx.set_line_dash(eval_once!(JsValue: js_array![number 10.0, number 10.0])).add_loc(loc!())?;
                    ctx.stroke_rect(origin[0], origin[1], cur.x as f64 - origin[0], cur.y as f64 - origin[1]);
                    ctx.set_line_dash(eval_once!(JsValue: js_array![])).add_loc(loc!())?;
                } else {
                    let [x, y] = self.last_event.point.map(|x| x as f64);
                    ctx.set_line_dash(eval_once!(JsValue: js_array![number 10.0, number 10.0])).add_loc(loc!())?;
                    ctx.begin_path();
                    ctx.move_to(-*size[0], y);
                    ctx.line_to( *size[0], y);
                    ctx.move_to(x, -*size[1]);
                    ctx.line_to(x,  *size[1]);
                    ctx.stroke();
                    ctx.set_line_dash(eval_once!(JsValue: js_array![])).add_loc(loc!())?;
                }
            }.pipe(|_| T::plane_hover_hint(|| confine(to_aligned_user(self.last_event.point)), *self.last_event)),

            Focus::Point{id, ..} =>
                T::point_hover_hint(unsafe{self.get_unchecked_aware(id)}, *self.last_event),

            Focus::Selection{..} =>
                T::selection_hover_hint(self.selection.iter().map(|x| unsafe{self.get_unchecked_aware(*x)}),
                    *self.last_event)
        })
    }
}
