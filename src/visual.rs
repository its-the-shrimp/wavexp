use std::{iter::Iterator, slice::from_raw_parts, mem::{Discriminant, discriminant}};

use js_sys::Array as JsArray;
use web_sys::{HtmlCanvasElement, AnalyserNode as JsAnalyserNode, ImageData as JsImageData, MouseEvent as JsMouseEvent, HtmlElement};
use wasm_bindgen::{Clamped as JsClamped, JsValue, JsCast};
use yew::{TargetCast, NodeRef};
use crate::{
    utils::{Check, SliceExt, Point,
        JsResult, HtmlCanvasExt, JsResultUtils,
        R32, R64, HitZone,
        HorizontalArrow, OptionExt,
        HtmlElementExt, VecExt, ResultToJsResult,
        Pipe, LooseEq, BoolExt, Tee, Take, Rect, document},
    sound::SoundGen,
    loc, input::ParamId, js_assert, r32
};

pub struct EveryNth<'a, T> {
    iter: &'a [T],
    n: usize,
    state: usize
}

impl<'a, T> Iterator for EveryNth<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
		let len = self.iter.len();
		let res = self.iter.get(self.state);
        self.state = (self.state + self.n)
            .check_not_in(len .. (len + self.n).saturating_sub(1))
            .unwrap_or_else(|x| x - len + 1);
        res
    }
}

pub struct EveryNthMut<'a, T> {
    iter: &'a mut [T],
    n: usize,
    state: usize
}

impl<'a, T> Iterator for EveryNthMut<'a, T> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<&'a mut T> {
		let len = self.iter.len();
		let res = self.iter.get_mut(self.state)
            .map(|x| unsafe{(x as *mut T).as_mut().unwrap_unchecked()});
        // the abomination above is there solely because such a trivial task as
        // a lending iterator over mutable data can't be done any other way
        self.state = (self.state + self.n)
            .check_not_in(len .. (len + self.n).saturating_sub(1))
            .unwrap_or_else(|x| x - len + 1);
        res
    }
}

pub trait ToEveryNth<T> {
    fn every_nth(&self, n: usize) -> EveryNth<'_, T>;
    fn every_nth_mut(&mut self, n: usize) -> EveryNthMut<'_, T>;
}

impl<T> ToEveryNth<T> for [T] {
    #[inline] fn every_nth(&self, n: usize) -> EveryNth<'_, T> {
        EveryNth {iter: self, n, state: 0}
    }
    #[inline] fn every_nth_mut(&mut self, n: usize) -> EveryNthMut<'_, T> {
        EveryNthMut {iter: self, n, state: 0}
    }
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

    // TODO: make it actually work
	pub fn poll(&mut self, input: &JsAnalyserNode) -> JsResult<()> {
		// TODO: correctly readjust the graph when shrinked in the UI
        let canvas: HtmlCanvasElement = self.canvas.cast().to_js_result(loc!())?;
        canvas.sync();
        let (new_width, new_height) = (canvas.width(), canvas.height());
        if new_width * new_height != self.width * self.height {
            self.width = new_width;
            self.height = new_height;
            self.in_data.resize(new_height as usize, 0);
            self.out_data.resize(new_width as usize * new_height as usize, Self::BG);
        }

		let len = self.out_data.len();
		self.out_data.copy_within(.. len - self.height as usize, self.height as usize);

        input.get_byte_frequency_data(&mut self.in_data);

		for (&src, dst) in self.in_data.iter().zip(self.out_data.every_nth_mut(self.width as usize)) {
            *dst = unsafe {*self.gradient.get_unchecked(src as usize)};
        }

        let out = JsClamped(unsafe{from_raw_parts(
            self.out_data.as_ptr().cast::<u8>(),
            self.out_data.len() * 4)});
        canvas.get_2d_context(loc!())?.put_image_data(
                &JsImageData::new_with_u8_clamped_array(out, self.width).add_loc(loc!())?,
                0.0, 0.0).add_loc(loc!())?;
        Ok(())
	}
}


#[derive(Debug, Clone, Copy)]
pub struct CanvasEvent {
    pub point: Point,
    pub left: bool,
    pub shift: bool
}

impl TryFrom<&JsMouseEvent> for CanvasEvent {
    type Error = JsValue;
    fn try_from(value: &JsMouseEvent) -> Result<Self, Self::Error> {
        let canvas: HtmlCanvasElement = value.target_dyn_into().to_js_result(loc!())?;
        let point = Point{x: value.offset_x(), y: value.offset_y()}
            .normalise(canvas.client_rect(), canvas.rect());
        Ok(Self{point, left: value.buttons() & 1 == 1, shift: value.shift_key()})
    }
}

pub struct GraphHandler {
    canvas: NodeRef,
    ratio: R32,
    event: Option<Option<CanvasEvent>>
}

impl GraphHandler {
    #[inline] pub fn new() -> JsResult<Self> {
        Ok(Self{canvas: NodeRef::default(), ratio: R32::INFINITY, event: None})
    }

    #[inline] pub fn canvas(&self) -> &NodeRef {
        &self.canvas
    }

    #[inline] pub fn set_event(&mut self, event: CanvasEvent) {
        self.event = Some(Some(event));
    }

    /// the returned `bool` indicates whether the selected element's editor window should be
    /// rerendered
    pub fn set_param(&mut self, id: ParamId, _value: R64) -> bool {
        if let ParamId::Select(_) = id {
            self.event = Some(None);
            true
        } else {false}
    }

    #[inline] pub fn poll(&mut self, element: Option<&mut SoundGen>) -> JsResult<()> {
        if let Some((Some(spec), element)) = element.map(|x| (x.graph_spec(), x)) {
            let canvas = self.canvas.cast::<HtmlCanvasElement>().to_js_result(loc!())?;
            if spec.ratio != self.ratio {
                if *self.ratio == 0.0 {
                    self.event = self.event.or(None);
                }
                canvas.set_height((canvas.width() as f32 * *spec.ratio) as u32);
                self.ratio = spec.ratio;
            }
            if !spec.interactive {
                self.event = None;
            }
            if let Some(event) = self.event.take().or_else(|| spec.force_redraw.then_some(None)) {
                let ctx = canvas.get_2d_context(loc!())?;
                let (w, h) = (canvas.width(), canvas.height());
                ctx.set_fill_style(&"#181818".into());
                ctx.set_stroke_style(&"#0069E1".into());
                ctx.set_line_width(3.0);
                ctx.fill_rect(0.0, 0.0, w as f64, h as f64);
                ctx.begin_path();
                element.graph(w, h, &ctx, event).add_loc(loc!())?;
                ctx.stroke();
            }
        } else if *self.ratio != 0.0 {
            self.ratio = r32![0.0];
            self.canvas.cast::<HtmlCanvasElement>().to_js_result(loc!())?.set_height(0);
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Focus {
    None,
    HoverPlane,
    HoverElement(usize),
    HoverElementConnecting(usize, usize),
    HoverConnector(usize),
    MovePlane(i32),
    MoveElement(usize, Point),
    ConnectElement(usize, Point),
}

pub struct EditorPlaneHandler {
    canvas: NodeRef,
    positions: Vec<Point>,
    selected_id: Option<usize>,
    focus: Focus,
    last_focus: Discriminant<Focus>,
    offset: Point,
    redraw: bool,
    solid_line: JsValue,
    dotted_line: JsValue,
}

impl EditorPlaneHandler {
    const ELEMENT_BB: Point = Point{x: 16, y: 16};
    const FONT: &str = "20px consolas";
    const BG_STYLE: &str = "#232328";
    const FG_STYLE: &str = "#0069E1";

    #[inline] pub fn new() -> JsResult<Self> {
        let doc = document();
        let canvas = doc.create_element("canvas").add_loc(loc!())?
            .unchecked_into::<HtmlCanvasElement>();
        let body = doc.body().to_js_result(loc!())?;
        canvas.set_width(body.client_width() as u32);
        canvas.set_height(body.client_height() as u32);

        let ctx = canvas.get_2d_context(loc!())?;
        ctx.set_text_align("middle");
        ctx.set_text_baseline("top");
        ctx.set_fill_style(&Self::BG_STYLE.into());
        ctx.set_stroke_style(&Self::FG_STYLE.into());
        ctx.set_line_width(3.0);
        ctx.set_font(Self::FONT);
        Ok(Self{canvas: NodeRef::default(),
            positions: vec![], selected_id: None,
            offset: Point::ZERO, redraw: true,
            focus: Focus::None, last_focus: discriminant(&Focus::HoverPlane),
            solid_line: JsArray::new().into(),
            dotted_line: JsArray::of2(&JsValue::from(10.0), &JsValue::from(10.0)).into()})
    }

    #[inline] pub fn canvas(&self) -> &NodeRef {
        &self.canvas
    }
    
    #[inline] pub fn add_element(&mut self, pos: Point) -> JsResult<()> {
        self.positions.push(pos);
        self.redraw = true;
        Ok(())
    }

    #[inline] pub fn remove_element(&mut self, id: usize) -> JsResult<()> {
        self.positions.try_swap_remove(id).to_js_result(loc!())?;
        self.redraw = true;
        Ok(())
    }

    /// the returned `bool` indicates whether the selected element's editor window should be
    /// rerendered
    pub fn set_param(&mut self, id: ParamId, _value: R64) -> JsResult<bool> {
        match id {
            ParamId::Remove(id) => self.remove_element(id).map(|_| true),
            ParamId::Add(_, pos) => self.add_element(pos).map(|_| false),
            ParamId::Select(_) => Ok(true),
            _ => Ok(false)
        }
    }

    #[inline] pub fn selected_element_id(&self) -> Option<usize> {
        self.selected_id
    }

    #[inline] fn in_conn_creation_area(element_pos: Point, point: Point) -> bool {
        HorizontalArrow::new(element_pos, Self::ELEMENT_BB.x, Self::ELEMENT_BB.x, true)
            .shift_x(Self::ELEMENT_BB.x) 
            .contains(point)
    }

    #[inline] fn id_by_pos(&mut self, point: Point) -> Option<usize> {
        self.positions.iter().position(|x| x.loose_eq(point, Self::ELEMENT_BB))
    }

    #[inline] fn set_focus(&mut self, focus: Focus) {
        self.focus = focus;
        self.redraw = true;
    }

    pub fn set_event(&mut self, event: Option<CanvasEvent>) -> Option<(ParamId, R64)> {
        let Some(mut event) = event else {
            self.focus = Focus::None;
            self.last_focus = discriminant(&self.focus);
            return None
        };
        event.point += self.offset;
        match self.focus {
            Focus::None => self.set_focus(Focus::HoverPlane).pipe(|_| None),
            Focus::HoverPlane => match (event.left, self.id_by_pos(event.point)) {
                (true, None) => Some(Focus::MovePlane(event.point.x - self.offset.x)),
                (true, Some(id)) =>
                    Some(Self::in_conn_creation_area(unsafe{*self.positions.get_unchecked(id)}, event.point)
                        .choose(Focus::ConnectElement(id, event.point), Focus::MoveElement(id, event.point))),
                (false, None) => None,
                (false, Some(id)) => Some(Focus::HoverElement(id))
            }.map(|x| self.set_focus(x)).pipe(|_| None),

            Focus::HoverElement(id) => {
                let pos = unsafe{self.positions.get_unchecked(id)};
                match (event.left, Self::in_conn_creation_area(*pos, event.point)) {
                    (true, true) => Some(Focus::ConnectElement(id, event.point)),
                    (true, false) => Some(Focus::MoveElement(id, event.point)),
                    (false, true) => Some(Focus::HoverConnector(id)),
                    (false, false) => pos.loose_ne(event.point, Self::ELEMENT_BB)
                        .then_some(Focus::HoverPlane)
                }.map(|x| self.set_focus(x));
                None
            }

            Focus::HoverElementConnecting(src_id, dst_id) => {
                let dst = unsafe{self.positions.get_unchecked(dst_id)};
                self.redraw = true;
                match (event.left, dst.loose_eq(event.point, Self::ELEMENT_BB)) {
                    (true, true) => (),
                    (true, false) =>
                        self.focus = Focus::ConnectElement(src_id, event.point),
                    (false, false) =>
                        self.focus = Focus::HoverPlane,
                    (false, true) => {
                        self.focus = Focus::HoverElement(dst_id);
                        self.selected_id = Some(dst_id);
                        return Some((ParamId::Connect(src_id, dst_id), R64::INFINITY))
                    }
                };
                None
            }

            Focus::HoverConnector(id) => {
                let pos = unsafe{self.positions.get_unchecked(id)};
                match (event.left, Self::in_conn_creation_area(*pos, event.point)) {
                    (true, true) => Some(Focus::ConnectElement(id, event.point)),
                    (true, false) => Some(Focus::MoveElement(id, event.point)),
                    (false, true) => None,
                    (false, false) => Some(pos.loose_ne(event.point, Self::ELEMENT_BB)
                        .choose(Focus::HoverElement(id), Focus::HoverPlane))
                }.map(|x| self.set_focus(x));
                None
            }

            Focus::MovePlane(ref mut last_x) => if event.left {
                event.point.x -= self.offset.x;
                self.offset.x += *last_x - event.point.x;
                *last_x = event.point.x;
            } else {
                self.focus = Focus::HoverPlane;
            }.pipe(|_| {self.redraw = true; None}),

            Focus::MoveElement(id, ref mut point) => if event.left {
                *unsafe{self.positions.get_unchecked_mut(id)} += event.point - *point;
                *point = event.point;
                None
            } else {
                self.focus = Focus::HoverElement(id);
                self.selected_id = (self.selected_id != Some(id)).then_some(id);
                Some((ParamId::Select(self.selected_id), R64::INFINITY))
            }.tee(|_| self.redraw = true),

            Focus::ConnectElement(id, ref mut point) => {
                self.redraw = true;
                *point = event.point;
                match (event.left, self.id_by_pos(event.point).filter(|x| *x != id)) {
                    (true, None) => (),
                    (true, Some(id2)) =>
                        self.focus = Focus::HoverElementConnecting(id, id2),
                    (false, None) =>
                        self.focus = Focus::HoverPlane,
                    (false, Some(id2)) => {
                        self.focus = Focus::HoverElement(id2);
                        self.selected_id = Some(id2);
                        return Some((ParamId::Connect(id, id2), R64::INFINITY))
                    }
                }
                None
            }
        }
    }

    pub fn poll(&mut self, elements: &[SoundGen], conns: &[Vec<usize>], hint_handler: &HintHandler) -> JsResult<()> {
        if !self.redraw.take() {return Ok(())}
        js_assert!(self.positions.len() == elements.len())?;
        let canvas = self.canvas.cast::<HtmlCanvasElement>().to_js_result(loc!())?;
        let change_hint = if self.last_focus != discriminant(&self.focus) {
            let body = document().body().to_js_result(loc!())?;
            canvas.set_width(body.client_width() as u32);
            canvas.set_height(body.client_height() as u32);
            let ctx = canvas.get_2d_context(loc!())?;
            ctx.set_text_align("middle");
            ctx.set_text_baseline("top");
            ctx.set_fill_style(&Self::BG_STYLE.into());
            ctx.set_stroke_style(&Self::FG_STYLE.into());
            ctx.set_line_width(3.0);
            ctx.set_font(Self::FONT);
            self.last_focus = discriminant(&self.focus);
            true
        } else {false};

        let ctx = canvas.get_2d_context(loc!())?;
        let (w, h) = (canvas.width() as f64, canvas.height() as f64);
        ctx.set_fill_style(&Self::BG_STYLE.into());
        ctx.fill_rect(0.0, 0.0, w, h);
        ctx.set_fill_style(&Self::FG_STYLE.into());
        ctx.begin_path();
        for (&(mut pos), conns) in self.positions.iter().zip(conns) {
            pos -= self.offset;
            Rect::center(pos, Self::ELEMENT_BB).draw(&ctx);
            let src = pos + Point{x: Self::ELEMENT_BB.x, y: 0};
            for &dst_id in conns {
                ctx.move_to(src.x as f64, src.y as f64);
                let dst = unsafe{*self.positions.get_unchecked(dst_id)}
                    - self.offset - Point{x: Self::ELEMENT_BB.x, y: 0};
                ctx.line_to(dst.x as f64, dst.y as f64);
            }
        }
        ctx.stroke();

        #[inline]
        unsafe fn fmt_element(prefix: &str, elements: &[SoundGen], id: usize, postfix: &str) -> String {
            format!("{prefix}{} [ID: {id}]{postfix}", elements.get_unchecked(id).name())
        }

        match self.focus {
            Focus::None => (),
            Focus::HoverPlane => if change_hint {
                hint_handler.set_hint("Editor plane", "").add_loc(loc!())?;
            }

            Focus::HoverElement(id) => if change_hint {
                unsafe{hint_handler.set_hint(
                    &fmt_element("", elements, id, ""),
                    "Press and hold to start moving")}.add_loc(loc!())?
            }

            Focus::HoverConnector(id) => if change_hint {
                unsafe{hint_handler.set_hint(
                    &fmt_element("", elements, id, ""),
                    "Press and hold to start connecting")}.add_loc(loc!())?
            }

            Focus::HoverElementConnecting(src_id, dst_id) => {
                if change_hint {
                    unsafe{hint_handler.set_hint(
                        &fmt_element("", elements, src_id, ": connecting"),
                        &fmt_element("Release to connect to ", elements, dst_id, "")
                    )}.add_loc(loc!())?
                }
                ctx.set_line_dash(&self.dotted_line).add_loc(loc!())?;
                let src = unsafe{*self.positions.get_unchecked(src_id)}
                    - self.offset + Point{x: Self::ELEMENT_BB.x, y: 0};
                ctx.move_to(src.x as f64, src.y as f64);
                let dst = unsafe{*self.positions.get_unchecked(dst_id)}
                    - self.offset - Point{x: Self::ELEMENT_BB.x, y: 0};
                ctx.line_to(dst.x as f64, dst.y as f64);
                ctx.stroke();
                ctx.set_line_dash(&self.solid_line).add_loc(loc!())?;
            }

            Focus::MovePlane(_) => if change_hint {
                hint_handler.set_hint("Editor plane: moving", "").add_loc(loc!())?;
            }

            Focus::MoveElement(id, _) => if change_hint {
                unsafe{hint_handler.set_hint(
                    &fmt_element("", elements, id, ": moving"),
                    "")}.add_loc(loc!())?
            }

            Focus::ConnectElement(id, point) => {
                if change_hint {
                    unsafe{hint_handler.set_hint(
                        &fmt_element("", elements, id, ": connecting"),
                        "Release to cancel")}.add_loc(loc!())?
                }
                ctx.set_line_dash(&self.dotted_line).add_loc(loc!())?;
                let src = unsafe{*self.positions.get_unchecked(id)}
                    - self.offset + Point{x: Self::ELEMENT_BB.x, y: 0};
                ctx.move_to(src.x as f64, src.y as f64);
                let dst = point - self.offset;
                ctx.line_to(dst.x as f64, dst.y as f64);
                ctx.stroke();
                ctx.set_line_dash(&self.solid_line).add_loc(loc!())?;
            }
        };
        Ok(())
    }
}

#[derive(PartialEq, Default)]
pub struct HintHandler {
    main_bar: NodeRef,
    aux_bar: NodeRef
}

impl HintHandler {
    const DEFAULT_MAIN: &str = "Here will be a hint";
    const DEFAULT_AUX: &str = "Hover over anything to get a hint";

    #[inline] pub fn set_hint(&self, main: &str, aux: &str) -> JsResult<()> {
        self.main_bar.cast::<HtmlElement>().to_js_result(loc!())?
            .set_inner_text(main);
        self.aux_bar.cast::<HtmlElement>().to_js_result(loc!())?
            .set_inner_text(aux);
        Ok(())
    }

    #[inline] pub fn clear_hint(&self) -> JsResult<()> {
        self.main_bar.cast::<HtmlElement>().to_js_result(loc!())?
            .set_inner_text(Self::DEFAULT_MAIN);
        self.aux_bar.cast::<HtmlElement>().to_js_result(loc!())?
            .set_inner_text(Self::DEFAULT_AUX);
        Ok(())
    }

    #[inline] pub fn main_bar(&self) -> &NodeRef {
        &self.main_bar
    }

    #[inline] pub fn aux_bar(&self) -> &NodeRef {
        &self.aux_bar
    }
}
