use std::{
    iter::{Iterator, successors as succ},
    slice::from_raw_parts,
    mem::{Discriminant, discriminant},
    ops::{Not, Div, Range, Deref},
    fmt::Debug
};
use web_sys::{
    HtmlCanvasElement,
    AnalyserNode,
    ImageData,
    MouseEvent,
    HtmlElement,
    Path2d};
use wasm_bindgen::{Clamped, JsValue};
use yew::{TargetCast, NodeRef};
use crate::{
    utils::{Check, SliceExt, Point,
        JsResult, HtmlCanvasExt, JsResultUtils, OptionExt,
        HtmlElementExt, 
        Pipe, Tee, Take, R32, RatioToInt, BoolExt, RangeExt, VecExt, ResultToJsResult, R64},
    sound::Beats,
    loc,
    r32, input::ParamId
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
	out_data: Vec<Rgba>,
	in_data: Vec<u8>,
    gradient: Vec<Rgba>,
    canvas: NodeRef,
    width: u32, height: u32
}

impl SoundVisualiser {
	pub const FG: Rgba = Rgba{r:0x00, g:0x69, b:0xE1, a:0xFF};
	pub const BG: Rgba = Rgba{r:0x18, g:0x18, b:0x18, a:0xFF};
	pub fn new() -> JsResult<Self> {
		Ok(Self{out_data: vec![], in_data: vec![],
            gradient: (0 ..= u8::MAX)
                .map(|i| interp(&[Self::BG, Self::FG], i))
                .collect(),
			width: 0, height: 0, canvas: NodeRef::default()})
	}

    #[inline] pub fn canvas(&self) -> &NodeRef {
        &self.canvas
    }

    // TODO: correctly readjust the graph when shrinked in the UI
    pub fn set_param(&mut self, id: ParamId, _value: R64) -> JsResult<bool> {
        if let ParamId::Resize = id {
            let canvas: HtmlCanvasElement = self.canvas.cast().to_js_result(loc!())?;
            let [w, h] = canvas.client_size().map(|x| x as u32);
            canvas.set_width(w);
            canvas.set_height(h);
            self.width = w;
            self.height = h;
            self.in_data.resize(w as usize, 0);
            self.out_data.resize(w as usize * w as usize, Self::BG);
        }
        Ok(false)
    }

	pub fn redraw(&mut self, input: &AnalyserNode) -> JsResult<()> {
        self.out_data.rotate_right(1);
        input.get_byte_frequency_data(&mut self.in_data);
        for (&src, dst) in self.in_data.iter().zip(self.out_data.every_nth_mut(self.width as usize)) {
            *dst = unsafe {*self.gradient.get_unchecked(src as usize)};
        }

        let out = unsafe{from_raw_parts(self.out_data.as_ptr().cast(), self.out_data.len() * 4)};
        let out = ImageData::new_with_u8_clamped_array(Clamped(out), self.width).add_loc(loc!())?;
        self.canvas.cast::<HtmlCanvasElement>().to_js_result(loc!())?.get_2d_context(loc!())?
            .put_image_data(&out, 0.0, 0.0).add_loc(loc!())?;
        Ok(())
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

#[derive(PartialEq, Default)]
pub struct HintHandler {
    main_bar: NodeRef,
    aux_bar: NodeRef
}

impl HintHandler {
    #[inline] pub fn set_hint(&self, main: &str, aux: &str) -> JsResult<()> {
        self.main_bar.cast::<HtmlElement>().to_js_result(loc!())?
            .set_inner_text(main);
        self.aux_bar.cast::<HtmlElement>().to_js_result(loc!())?
            .set_inner_text(aux);
        Ok(())
    }

    #[inline] pub fn main_bar(&self) -> &NodeRef {
        &self.main_bar
    }

    #[inline] pub fn aux_bar(&self) -> &NodeRef {
        &self.aux_bar
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Focus {
    None,
    HoverPlane,
    HoverBlock(usize),
    MovePlane(Point),
    MoveBlock(usize, Point),
    HoverPlaneWShift(Point),
    ZoomPlane{init_offset: Point, pivot: Point, init_scale_x: R32, init_scale_y: R32}
}

impl Focus {
    #[inline] pub fn always_update(&self) -> bool {
        matches!(self, Self::MoveBlock(..)
            | Self::ZoomPlane{..}
            | Self::HoverPlaneWShift(..))
    }
}

/// data that can be edited with a generic graph editor defined below
pub trait Graphable: Debug {
    /// bounds for the scale of the X axis of the graph
    const SCALE_X_BOUND: Range<R32>;
    /// bounds for the scale of the Y axis of the graph
    const SCALE_Y_BOUND: Range<R32>;
    /// type of the inner data of the point, can be whatever and,
    /// unlike the coordinates, can be freely mutated
    type Inner;
    /// type returned from `GraphEditor::handle_hover` when the user interacted with the UI in a special way,
    /// i.e. selected a point
    type Event;

    /// inner data of the point
    fn inner(&self) -> &Self::Inner;
    /// mutable inner data of the point
    fn inner_mut(&mut self) -> &mut Self::Inner;
    /// location of the point in user coordinates
    fn loc(&self) -> [R32; 2];
    /// set the location of the point in user coordinates when moved in the UI
    fn set_loc(&mut self, n_points: usize, self_id: usize, x: impl FnOnce() -> R32, y: impl FnOnce() -> R32);
    /// returns `true` if the given user coordinates are inside the hitbox of the point
    fn in_hitbox(&self, point: [R32; 2]) -> bool;
    /// the message to send when the point is selected/de-selected in the UI
    fn on_select(self_id: Option<usize>) -> Option<Self::Event>;
    /// visual representation of the point's hitbox
    /// `mapper` maps user coordinates to canvas coordinates, all vertices in the
    /// returned `Path2d` must be the result of the `mapper` function
    fn draw(&self, next: Option<&Self>, mapper: impl Fn([f64; 2]) -> [f64; 2]) -> JsResult<Path2d>;
    // return type of `desc` and `fmt_loc` should be `impl AsRef<str>`
    // but feature `return_position_impl_trait_in_trait` is not usable yet
    /// description of the point that'll be shown in the hint handler when the point's hovered over
    fn desc(&self) -> String;
    /// format the location of a point in user coordinates
    fn fmt_loc(loc: [R32; 2]) -> String;
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

#[derive(Debug, Clone, PartialEq)]
pub struct GraphEditor<T: Graphable> {
    canvas: NodeRef,
    offset: Point,
    scale_x: R32,
    scale_y: R32,
    snap_step: R32,
    focus: Focus,
    last_focus: Discriminant<Focus>,
    redraw: bool,
    data: Vec<T>
}

impl<T: Graphable> GraphEditor<T> {
    const FONT: &str = "20px consolas";
    const BG_STYLE: &str = "#232328";
    const MG_STYLE: &str = "#333338";
    const FG_STYLE: &str = "#0069E1";
    const LINE_WIDTH: f64 = 3.0;

    #[inline] pub fn new(scale_x: R32, scale_y: R32) -> Self {
        Self{canvas: NodeRef::default(), offset: Point::ZERO,
            focus: Focus::None, last_focus: discriminant(&Focus::HoverPlane),
            scale_x, scale_y, snap_step: r32![1.0],
            redraw: false, data: vec![]}
    }

    #[inline] pub fn canvas(&self) -> &NodeRef {&self.canvas}

    #[inline] pub fn data(&self) -> &[T] {&self.data}

    #[inline] pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        self.data.get_unchecked(index)
    }

    #[inline] pub unsafe fn get_unchecked_mut(&mut self, index: usize)
    -> GraphPointView<'_, T> {
        GraphPointView(self.data.get_unchecked_mut(index))
    }

    #[inline] pub fn get(&self, index: usize) -> Option<&T> {
        self.data.get(index)
    }

    #[inline] pub fn get_mut(&mut self, index: usize) -> Option<GraphPointView<'_, T>> {
        self.data.get_mut(index).map(GraphPointView)
    }

    #[inline] pub fn iter_mut(&mut self) -> impl Iterator<Item=GraphPointView<'_, T>> {
        self.data.iter_mut().map(GraphPointView)
    }

    /// returns the index at which `point` was placed
    #[inline] pub fn add_point(&mut self, point: T) -> usize {
        self.redraw = true;
        self.data.push_sorted_by_key(point, |x| x.loc()[0])
    }

    #[inline] pub fn del_point(&mut self, index: usize) -> JsResult<()> {
        self.redraw = true;
        self.data.try_remove(index).to_js_result(loc!()).map(|_| ())
    }

    #[inline] pub fn snap_step(&self) -> R32 {
        self.snap_step
    }

    #[inline] pub fn set_snap_step(&mut self, snap_step: R32) {
        self.snap_step = snap_step;
    }

    #[inline] pub fn force_redraw(&mut self) {
        self.redraw = true;
    }

    #[inline] fn id_by_pos(x: R32, y: R32, pattern: &[T]) -> Option<usize> {
        pattern.iter().position(|point| point.in_hitbox([x, y]))
    }

    #[inline] fn set_focus(&mut self, focus: Focus) {
        self.focus = focus;
        self.redraw = true;
    }

    pub fn handle_hover(&mut self, event: Option<CanvasEvent>) -> Option<T::Event> {
        let Some(mut event) = event else {
            self.focus = Focus::None;
            self.last_focus = discriminant(&self.focus);
            return None
        };

        event.point += self.offset;
        let [w, h]   = self.canvas.cast::<HtmlCanvasElement>()?.size();
        let step_x   = R32::from(w) / self.scale_x;
        let step_y  = R32::from(h) / self.scale_y;
        let x        = R32::from(event.point.x) / step_x;
        let y        = R32::from(event.point.y) / step_y;

        let move_element_focus = |id: usize| {
            let off = unsafe{self.data.get_unchecked(id)}.loc()[0];
            Focus::MoveBlock(id, event.point.shift_x((-step_x * off).to_int()))
        };

        let zoom_plane_focus = || Focus::ZoomPlane{
            init_offset: self.offset, pivot: event.point,
            init_scale_x: self.scale_x, init_scale_y: self.scale_y};

        match self.focus {
            Focus::HoverPlane => match (event.left, event.shift) {
                (true, true) =>
                    Some(zoom_plane_focus()),
                (true, false) =>
                    Self::id_by_pos(x, y, &self.data)
                        .map_or_else(|| Focus::MovePlane(event.point - self.offset),
                            move_element_focus).into(),
                (false, true) =>
                    Some(Focus::HoverPlaneWShift(event.point)),
                (false, false) =>
                    Self::id_by_pos(x, y, &self.data).map(Focus::HoverBlock)
            }.map(|x| self.set_focus(x)).pipe(|_| None),

            Focus::HoverBlock(id) => match (event.left, event.shift) {
                (true, true) => Some(zoom_plane_focus()),
                (true, false) => Some(move_element_focus(id)),
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

            Focus::MoveBlock(id, init_offset) => if event.left {
                let n_points = self.data.len();
                unsafe{self.data.get_unchecked_mut(id)}.set_loc(n_points, id,
                    || R32::from(event.point.x - init_offset.x).div(step_x)
                        .max(r32![0.0]).round_to(self.snap_step),
                    || (R32::from(event.point.y - init_offset.y) / step_y).max(R32::ZERO));
                None
            } else {
                self.focus = Focus::HoverBlock(id);
                T::on_select(Some(id))
            }.tee(|_| self.redraw = true),

            Focus::HoverPlaneWShift(ref mut point) => match (event.left, event.shift) {
                (true, true) => 
                    self.focus = zoom_plane_focus(),
                (true, false) =>
                    self.focus = Self::id_by_pos(x, y, &self.data)
                        .map_or_else(|| Focus::MovePlane(event.point - self.offset),
                            move_element_focus),
                (false, true) => *point = event.point,
                (false, false) => self.focus = Focus::HoverPlane,
            }.pipe(|_| {self.redraw = true; None}),

            Focus::ZoomPlane{init_offset, init_scale_x, init_scale_y, pivot} => match (event.left, event.shift) {
                (true, _) => {
                    event.point -= self.offset - init_offset;

                    self.scale_x = T::SCALE_X_BOUND.fit(r32![50.0] / h * (event.point.x - pivot.x) + init_scale_x);
                    self.offset.x = ((init_scale_x - self.scale_x) * pivot.x / self.scale_x + init_offset.x)
                        .to_int();

                    self.scale_y = T::SCALE_Y_BOUND.fit(r32![50.0] / w * (event.point.y - pivot.y) + init_scale_y);
                    self.offset.y = ((init_scale_y - self.scale_y) * pivot.y / self.scale_y + init_offset.y)
                        .to_int();
                }

                (false, true) => self.focus = Focus::HoverPlaneWShift(event.point),

                (false, false) => self.focus = Focus::HoverPlane
            }.pipe(|_| {self.redraw = true; None}),

            _ => self.set_focus(Focus::HoverPlane).pipe(|_| None),
        }
    }

    /// if no canvas is bound to the editor, this is a no-op
    pub fn handle_resize(&mut self) {
        let Some(canvas) = self.canvas.cast::<HtmlCanvasElement>()
        else {return};
        canvas.set_width(canvas.client_width() as u32);
        canvas.set_height(canvas.client_height() as u32);
    }

    pub fn redraw(&mut self, hint_handler: &HintHandler, play_offset: Beats) -> JsResult<()> {
        if !self.redraw.take() && play_offset.is_infinite() {return Ok(())}

        let canvas: HtmlCanvasElement = self.canvas().cast().to_js_result(loc!())?;
        let [w, h] = canvas.size().map(|x| x as f64);
        let ctx = canvas.get_2d_context(loc!())?;

        ctx.set_fill_style(&Self::BG_STYLE.into());
        ctx.fill_rect(0.0, 0.0, w, h);

        let step_x = w / *self.scale_x as f64;
        let step_y = h / *self.scale_y as f64;
        let offset_x = -self.offset.x as f64
            % (self.offset.x > 0).choose(step_x, f64::INFINITY);
        let offset_y = -self.offset.y as f64
            % (self.offset.y > 0).choose(step_y * 2.0, f64::INFINITY);

        // lighter horizontal bars
        ctx.begin_path();
        for y in succ(Some(offset_y), |y| (y + step_y * 2.0).check_in(..h).ok()) {
            for x in succ(Some(offset_x), |x| (x + step_x + Self::LINE_WIDTH).check_in(..w).ok()) {
                ctx.rect(x, y, step_x, step_y);
            }
        }
        // lighter vertical lines
        for y in succ(Some(step_y + offset_y), |y| (y + step_y * 2.0).check_in(..h).ok()) {
            for x in succ(Some(offset_x), |x| (x + step_x + Self::LINE_WIDTH).check_in(..w).ok()) {
                ctx.rect(x - Self::LINE_WIDTH, y, Self::LINE_WIDTH, step_y);
            }
        }
        ctx.set_fill_style(&Self::MG_STYLE.into());
        ctx.fill();

        ctx.begin_path();
        let mapper = |[x, y]: [f64; 2]| [x * step_x - self.offset.x as f64, y * step_y - self.offset.y as f64];
        for [this, next] in self.data.array_windows::<2>() {
            this.draw(Some(next), mapper).add_loc(loc!())?;
        }
        if let Some(last) = self.data.last() {
            last.draw(None, mapper).add_loc(loc!())?;
        }
        ctx.fill();
        ctx.set_stroke_style(&Self::FG_STYLE.into());
        ctx.stroke();

        if discriminant(&self.focus) != self.last_focus || self.focus.always_update() {
            self.last_focus = discriminant(&self.focus);
            match self.focus {
                Focus::None => {
                    ctx.set_font(Self::FONT);
                    ctx.set_line_width(Self::LINE_WIDTH);
                }

                Focus::HoverPlane => hint_handler
                    .set_hint("Editor plane", "").add_loc(loc!())?,

                Focus::HoverBlock(id) => {
                    let point = self.data.get(id).to_js_result(loc!())?;
                    hint_handler
                        .set_hint(&format!("{}@{}", &point.desc(), &T::fmt_loc(point.loc())),
                            "Press and hold to drag")
                        .add_loc(loc!())?;
                }

                Focus::MovePlane(_) => hint_handler
                    .set_hint("Editor plane", "Dragging").add_loc(loc!())?,

                Focus::MoveBlock(id, _) => {
                    let point = unsafe{self.data.get_unchecked(id)};
                    hint_handler
                        .set_hint(&format!("{}@{}", &point.desc(), &T::fmt_loc(point.loc())),
                            "Dragging")
                        .add_loc(loc!())?;
                }

                Focus::HoverPlaneWShift(at) => {
                    hint_handler.set_hint("Editor plane", "Press and hold to zoom")
                        .add_loc(loc!())?;
                    let x = R32::from(at.x) / step_x;
                    let y = R32::from(at.y) / step_y;
                    ctx.set_text_align("left");
                    ctx.set_text_baseline("bottom");
                    ctx.set_fill_style(&Self::FG_STYLE.into());
                    ctx.fill_text(&T::fmt_loc([x, y]), 5.0, h - 5.0)
                        .add_loc(loc!())?;
                }

                Focus::ZoomPlane{pivot, init_offset, ..} => {
                    hint_handler.set_hint("Editor plane: zooming", "Release to stop")
                        .add_loc(loc!())?;
                    let [x, y] = (pivot - init_offset).map(|x| x as f64);
                    ctx.begin_path();
                    ctx.move_to(x - 10.0, y);
                    ctx.line_to(x + 10.0, y);
                    ctx.move_to(x, y - 10.0);
                    ctx.line_to(x, y + 10.0);
                    ctx.set_stroke_style(&Self::FG_STYLE.into());
                    ctx.stroke();
                }
            }
        }

        ctx.set_fill_style(&Self::FG_STYLE.into());
        ctx.fill_rect(*play_offset * step_x - self.offset.x as f64, 0.0, 3.0, h);
        Ok(())
    }
}
