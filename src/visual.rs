use std::{
    iter::{Iterator, successors as succ},
    slice::from_raw_parts,
    mem::{Discriminant, discriminant},
    rc::Rc,
    ops::{Mul, Neg, Not}
};
use web_sys::{HtmlCanvasElement, AnalyserNode, ImageData, MouseEvent, HtmlElement, Event};
use wasm_bindgen::{Clamped, JsValue};
use yew::{TargetCast, NodeRef, html, Html};
use crate::{
    utils::{Check, SliceExt, Point,
        JsResult, HtmlCanvasExt, JsResultUtils, R64, OptionExt,
        HtmlElementExt, 
        Pipe, Tee, document, Take, HtmlDocumentExt, R32, RatioToInt},
    loc, r64,
    input::{ParamId, Switch},
    sequencer::PatternBlock,
    sound::{Beats, SoundType}, r32
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

    // TODO: correctly readjust the graph when shrinked in the UI
    pub fn handle_resize(&mut self) -> JsResult<()> {
        let canvas: HtmlCanvasElement = self.canvas.cast().to_js_result(loc!())?;
        let [w, h] = canvas.client_size().map(|x| x as u32);
        canvas.set_width(w);
        canvas.set_height(h);
        self.width = w;
        self.height = h;
        self.in_data.resize(w as usize, 0);
        self.out_data.resize(w as usize * w as usize, Self::BG);
        Ok(())
    }

    // TODO: make it actually work
	pub fn poll(&mut self, input: Option<&AnalyserNode>) -> JsResult<()> {
        let canvas: HtmlCanvasElement = self.canvas.cast().to_js_result(loc!())?;
        if let Some(input) = input {
            let len = self.out_data.len();
            self.out_data.copy_within(.. len - self.height as usize, self.height as usize);
            input.get_byte_frequency_data(&mut self.in_data);
            for (&src, dst) in self.in_data.iter().zip(self.out_data.every_nth_mut(self.width as usize)) {
                *dst = unsafe {*self.gradient.get_unchecked(src as usize)};
            }
            let out = Clamped(unsafe{from_raw_parts(
                self.out_data.as_ptr().cast::<u8>(),
                self.out_data.len() * 4)});
            canvas.get_2d_context(loc!())?.put_image_data(
                    &ImageData::new_with_u8_clamped_array(out, self.width).add_loc(loc!())?,
                    0.0, 0.0).add_loc(loc!())?;
        }

        Ok(())
	}
}


#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Focus {
    None,
    HoverPlane,
    HoverElement(usize),
    MovePlane(Point),
    MoveElement(usize, Point),
    AwaitNewBlock(SoundType),
    MoveNewBlock(SoundType, Point),
    HoverPlaneWShift(Point),
    ZoomPlane{init_offset: Point, pivot: Point, init_scale_x: Beats, init_scale_y: R32}
}

impl Focus {
    #[inline] pub fn always_update(&self) -> bool {
        matches!(self, Self::MoveNewBlock(..)
            | Self::MoveElement(..)
            | Self::ZoomPlane{..}
            | Self::HoverPlaneWShift(..))
    }
}

pub struct EditorPlaneHandler {
    canvas: NodeRef,
    selected_id: Option<usize>,
    focus: Focus,
    last_focus: Discriminant<Focus>,
    offset: Point,
    redraw: bool,
    scale_x: Beats,
    scale_y: R32,
    snap_step: R64
}

impl EditorPlaneHandler {
    const FONT: &str = "20px consolas";
    const BG_STYLE: &str = "#232328";
    const MG_STYLE: &str = "#333338";
    const FG_STYLE: &str = "#0069E1";
    const LINE_WIDTH: f64 = 3.0;

    #[inline] pub fn new() -> JsResult<Self> {
        Ok(Self{canvas: NodeRef::default(), selected_id: None,
            offset: Point::ZERO, redraw: true,
            focus: Focus::None, last_focus: discriminant(&Focus::HoverPlane),
            scale_x: r64![20.0], scale_y: r32![10.0], snap_step: r64![1.0]})
    }

    #[inline] pub fn canvas(&self) -> &NodeRef {
        &self.canvas
    }
    
    #[inline] pub fn selected_element_id(&self) -> Option<usize> {
        self.selected_id
    }

    pub fn params(&self, hint: &Rc<HintHandler>) -> Html {
        html!{
            <div id="plane-settings" onpointerover={hint.setter("Editor plane settings", "")}>
                <Switch {hint} key="snap" name="Interval for blocks to snap to"
                    id={ParamId::SnapStep}
                    options={vec!["None", "1", "1/2", "1/4", "1/8"]}
                    initial={match *self.snap_step {
                        x if x == 1.0   => 1,
                        x if x == 0.5   => 2,
                        x if x == 0.25  => 3,
                        x if x == 0.125 => 4,
                        _ => 0
                    }}/>
            </div>
        }
    }

    /// the returned `bool` indicates whether the selected block's editor window should be
    /// rerendered
    pub fn set_param(&mut self, id: ParamId, value: R64) -> bool {
        match id {
            ParamId::SnapStep => {
                self.snap_step = *[r64![0.0], r64![1.0], r64![0.5], r64![0.25], r64![0.125]]
                    .get_wrapping(*value as usize);
                false
            }

            ParamId::Select(id) => {
                self.selected_id = id;
                true
            }

            id => id.need_plane_rerender().then(|| self.redraw = true).is_some()
        }
    }

    pub fn handle_resize(&mut self) -> JsResult<()> {
        let canvas: HtmlCanvasElement = self.canvas().cast().to_js_result(loc!())?;
        let doc = document();
        let w = doc.body().to_js_result(loc!())?.client_width()
            - doc.element_dyn_into::<HtmlElement>("ctrl-panel", loc!())?.client_width();
        canvas.set_width(w as u32);
        let h = canvas.client_height();
        canvas.set_height(h as u32);
        let ctx = canvas.get_2d_context(loc!())?;
        ctx.set_line_width(Self::LINE_WIDTH);
        ctx.set_font(Self::FONT);
        self.redraw = true;
        Ok(())
    }

    #[inline] fn in_block(block: &PatternBlock, offset: Beats, layer: i32) -> bool {
        layer == block.layer
            && (*block.offset .. *block.offset + *block.sound.len()).contains(&*offset)
    }

    #[inline] fn id_by_pos(offset: Beats, layer: i32, pattern: &[PatternBlock]) -> Option<usize> {
        pattern.iter().position(|x| Self::in_block(x, offset, layer))
    }

    #[inline] fn set_focus(&mut self, focus: Focus) {
        self.focus = focus;
        self.redraw = true;
    }

    pub fn handle_hover(&mut self, event: Option<CanvasEvent>, pattern: &mut [PatternBlock])
    -> Option<(ParamId, R64)> {
        let Some(mut event) = event else {
            self.focus = Focus::None;
            self.last_focus = discriminant(&self.focus);
            return None
        };

        event.point += self.offset;
        let [w, h] = self.canvas.cast::<HtmlCanvasElement>()?.size();
        let beat_w  = Beats::from(w) / self.scale_x;
        let layer_h = R32::from(h) / self.scale_y;
        let offset  = Beats::from(event.point.x) / beat_w;
        let layer   = (R32::from(event.point.y) / layer_h).to_int();

        let move_element_focus = |id: usize| {
            let off = unsafe{pattern.get_unchecked(id)}.offset;
            Focus::MoveElement(id, event.point.shift_x(off.mul(beat_w).neg().to_int()))
        };

        let zoom_plane_focus = || Focus::ZoomPlane{
            init_offset: self.offset, pivot: event.point,
            init_scale_x: self.scale_x, init_scale_y: self.scale_y};

        match self.focus {
            Focus::HoverPlane => match (event.left, event.shift) {
                (true, true) =>
                    Some(Focus::ZoomPlane{init_offset: self.offset, pivot: event.point,
                        init_scale_x: self.scale_x, init_scale_y: self.scale_y}),
                (true, false) =>
                    Self::id_by_pos(offset, layer, pattern)
                        .map_or_else(|| Focus::MovePlane(event.point - self.offset),
                            move_element_focus).into(),
                (false, true) =>
                    Some(Focus::HoverPlaneWShift(event.point)),
                (false, false) =>
                    Self::id_by_pos(offset, layer, pattern)
                        .map(Focus::HoverElement)
            }.map(|x| self.set_focus(x)).pipe(|_| None),

            Focus::HoverElement(id) => match (event.left, event.shift) {
                (true, true) => Some(zoom_plane_focus()),
                (true, false) => Some(move_element_focus(id)),
                (false, true) => Some(Focus::HoverPlaneWShift(event.point)),
                (false, false) =>
                    Self::in_block(unsafe{pattern.get_unchecked(id)}, offset, layer)
                        .not().then_some(Focus::HoverPlane),
            }.map(|x| self.set_focus(x)).pipe(|_| None),

            Focus::MovePlane(ref mut last) => if event.left {
                event.point -= self.offset;
                self.offset.x = (self.offset.x + last.x - event.point.x).max(self.offset.x.min(0));
                self.offset.y = (self.offset.y + last.y - event.point.y).max(self.offset.y.min(0));
                *last = event.point;
                None
            } else {
                self.focus = Focus::HoverPlane;
                Some((ParamId::Select(None), R64::INFINITY))
            }.tee(|_| self.redraw = true),

            Focus::MoveElement(id, init_offset) => if event.left {
                let block = unsafe{pattern.get_unchecked_mut(id)};
                block.offset = (Beats::from(event.point.x - init_offset.x) / beat_w)
                    .max(Beats::ZERO);
                if *self.snap_step != 0.0 {
                    block.offset = (block.offset / self.snap_step).ceil() * self.snap_step;
                }
                block.layer = (R32::from(event.point.y) / layer_h).to_int_from(0);
                None
            } else {
                self.focus = Focus::HoverElement(id);
                Some((ParamId::Select((self.selected_id != Some(id)).then_some(id)), R64::INFINITY))
            }.tee(|_| self.redraw = true),

            Focus::HoverPlaneWShift(ref mut point) => match (event.left, event.shift) {
                (true, true) => 
                    self.focus = Focus::ZoomPlane{init_offset: self.offset, pivot: event.point,
                        init_scale_x: self.scale_x, init_scale_y: self.scale_y},
                (true, false) =>
                    self.focus = Self::id_by_pos(offset, layer, pattern)
                        .map_or_else(|| Focus::MovePlane(event.point - self.offset),
                            move_element_focus),
                (false, true) => *point = event.point,
                (false, false) => self.focus = Focus::HoverPlane,
            }.pipe(|_| {self.redraw = true; None}),

            Focus::ZoomPlane{init_offset, init_scale_x, init_scale_y, pivot} => match (event.left, event.shift) {
                (true, _) => {
                    event.point -= self.offset - init_offset;

                    self.scale_x = (init_scale_x + Beats::from(event.point.x - pivot.x) / h * 50i8)
                        .clamp(r64![5.0], r64![95.0]);
                    self.offset.x = ((init_scale_x - self.scale_x) * pivot.x / self.scale_x + init_offset.x)
                        .to_int();

                    self.scale_y = (R32::from(event.point.y - pivot.y) / w * 50i8 + init_scale_y)
                        .clamp(r32![5.0], r32![30.0]);
                    self.offset.y = ((init_scale_y - self.scale_y) * pivot.y / self.scale_y + init_offset.y)
                        .to_int();
                }

                (false, true) => self.focus = Focus::HoverPlaneWShift(event.point),

                (false, false) => self.focus = Focus::HoverPlane
            }.pipe(|_| {self.redraw = true; None}),

            _ => self.set_focus(Focus::HoverPlane).pipe(|_| None),
        }
    }

    pub fn handle_block_add(&mut self, ty: Option<SoundType>, at: Option<Point>) -> Option<(ParamId, R64)> {
        match (at, ty) {
            (None, None) => self.set_focus(Focus::None)
                .pipe(|_| None),

            (None, Some(ty)) => self.set_focus(Focus::AwaitNewBlock(ty))
                .pipe(|_| None),

            (Some(mut at), None) => if let Focus::MoveNewBlock(ty, _) = self.focus {
                at += self.offset;
                let [w, h] = self.canvas.cast::<HtmlCanvasElement>()?.size();
                let layer = (R32::from(at.y) / h * self.scale_y).to_int();
                let offset = Beats::from(at.x) / w * self.scale_x;
                Some((ParamId::Add(ty, layer), offset))
            } else {
                self.set_focus(Focus::None);
                None
            }

            (Some(new_at), Some(ty)) => if let Focus::MoveNewBlock(_, at) = &mut self.focus {
                *at = new_at + self.offset;
            } else {
                self.focus = Focus::MoveNewBlock(ty, new_at);
            }.pipe(|_| {self.redraw = true; None}),
        }
    }

    pub fn poll(&mut self, pattern: &[PatternBlock], hint_handler: &HintHandler) -> JsResult<()> {
        if !self.redraw.take() {return Ok(())}

        let canvas: HtmlCanvasElement = self.canvas().cast().to_js_result(loc!())?;
        let [w, h] = canvas.size().map(|x| x as f64);
        let ctx = canvas.get_2d_context(loc!())?;

        ctx.set_fill_style(&Self::BG_STYLE.into());
        ctx.fill_rect(0.0, 0.0, w, h);

        let step_x = w / *self.scale_x;
        let step_y = h / *self.scale_y as f64;
        let offset_x = if self.offset.x >= 0 {
            -self.offset.x as f64 % step_x
        } else {
            -self.offset.x as f64
        };
        let offset_y = if self.offset.y >= 0 {
            -self.offset.y as f64 % (step_y * 2.0)
        } else {
            -self.offset.y as f64
        };

        // lighter horizontal bars
        ctx.begin_path();
        for y in succ(Some(offset_y), |y| (y + step_y * 2.0).check_in(..h).ok()) {
            ctx.rect(offset_x, y, w + step_x, step_y);
        }
        // lighter vertical lines
        for y in succ(Some(step_y + offset_y), |y| (y + step_y * 2.0).check_in(..h).ok()) {
            for x in succ(Some(offset_x), |x| (x + step_x).check_in(..w).ok()) {
                ctx.rect(x - Self::LINE_WIDTH / 2.0, y, Self::LINE_WIDTH, step_y);
            }
        }
        ctx.set_fill_style(&Self::MG_STYLE.into());
        ctx.fill();

        // darker vertical lines
        ctx.begin_path();
        for y in succ(Some(offset_y), |y| (y + step_y * 2.0).check_in(..h).ok()) {
            let next_y = y + step_y;
            for x in succ(Some(offset_x), |x| (x + step_x).check_in(..w).ok()) {
                ctx.move_to(x, y);
                ctx.line_to(x, next_y);
            }
        }
        ctx.set_stroke_style(&Self::BG_STYLE.into());
        ctx.stroke();

        ctx.begin_path();
        let layer_h = h / *self.scale_y as f64;
        let beat_w = w / *self.scale_x;
        for block in pattern {
            ctx.rect(*block.offset * beat_w - self.offset.x as f64,
                block.layer as f64 * layer_h - self.offset.y as f64,
                *block.sound.len() * beat_w, layer_h);
        }
        ctx.fill();
        ctx.set_stroke_style(&Self::FG_STYLE.into());
        ctx.stroke();

        if discriminant(&self.focus) != self.last_focus || self.focus.always_update() {
            self.last_focus = discriminant(&self.focus);
            match self.focus {
                Focus::None => (),

                Focus::HoverPlane => hint_handler
                    .set_hint("Editor plane", "").add_loc(loc!())?,

                Focus::HoverElement(id) => hint_handler
                    .set_hint(&pattern.get(id).to_js_result(loc!())?.desc(),
                        "Press and hold to drag").add_loc(loc!())?,

                Focus::MovePlane(_) => hint_handler
                    .set_hint("Editor plane", "Dragging").add_loc(loc!())?,

                Focus::MoveElement(id, _) => hint_handler
                    .set_hint(&pattern.get(id).to_js_result(loc!())?.desc(),
                        "Dragging").add_loc(loc!())?,

                Focus::AwaitNewBlock(ty) => {
                    hint_handler.set_hint("Adding new block",
                        &format!("Drag {} into the editor plane to add it", ty.name()))
                        .add_loc(loc!())?;
                    ctx.begin_path();
                    ctx.rect(0.0, 0.0, w, 5.0);
                    ctx.rect(0.0, 0.0, 5.0, h);
                    ctx.rect(0.0, h - 5.0, w, 5.0);
                    ctx.rect(w - 5.0, 0.0, 5.0, h);
                    ctx.set_fill_style(&Self::FG_STYLE.into());
                    ctx.fill();
                    ctx.set_text_align("center");
                    ctx.set_text_baseline("middle");
                    ctx.fill_text("Drag here", w / 2.0, h / 2.0).add_loc(loc!())?;
                }

                Focus::MoveNewBlock(ty, at) => {
                    let layer = (at.y as f64 / layer_h) as i32;
                    let offset = Beats::from(at.x) / beat_w;
                    hint_handler.set_hint("Adding new block",
                        &format!("Release to add {}", ty.desc(offset, layer)))
                        .add_loc(loc!())?;

                    ctx.begin_path();
                    let mut y = layer as f64 * layer_h - self.offset.y as f64;
                    let     x = (at.x - self.offset.x) as f64;
                    ctx.move_to(x - 5.0, y);
                    ctx.line_to(x + 5.0, y);
                    ctx.move_to(x, y);
                    y += layer_h;
                    ctx.line_to(x, y);
                    ctx.move_to(x - 5.0, y);
                    ctx.line_to(x + 5.0, y);
                    ctx.stroke();

                    ctx.begin_path();
                    ctx.rect(0.0, 0.0, w, 5.0);
                    ctx.rect(0.0, 0.0, 5.0, h);
                    ctx.rect(0.0, h - 5.0, w, 5.0);
                    ctx.rect(w - 5.0, 0.0, 5.0, h);
                    ctx.set_fill_style(&Self::FG_STYLE.into());
                    ctx.fill();
                }

                Focus::HoverPlaneWShift(at) => {
                    hint_handler.set_hint("Editor plane", "Press and hold to zoom")
                        .add_loc(loc!())?;
                    let layer = (at.y as f64 / layer_h) as i32;
                    let offset = at.x as f64 / beat_w;
                    ctx.set_text_align("left");
                    ctx.set_text_baseline("bottom");
                    ctx.set_fill_style(&Self::FG_STYLE.into());
                    ctx.fill_text(&format!("@{offset:.3}, layer {layer}"),
                        5.0, h - 5.0).add_loc(loc!())?;
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
        Ok(())
    }
}

#[derive(PartialEq, Default)]
pub struct HintHandler {
    main_bar: NodeRef,
    aux_bar: NodeRef,
}

impl HintHandler {
    #[inline] pub fn set_hint(&self, main: &str, aux: &str) -> JsResult<()> {
        self.main_bar.cast::<HtmlElement>().to_js_result(loc!())?
            .set_inner_text(main);
        self.aux_bar.cast::<HtmlElement>().to_js_result(loc!())?
            .set_inner_text(aux);
        Ok(())
    }

    pub fn setter<T>(self: &Rc<Self>, main: impl AsRef<str>, aux: impl AsRef<str>)
    -> impl Fn(T) where T: AsRef<Event> {
        let res = Rc::clone(self);
        move |_| _ = res.set_hint(main.as_ref(), aux.as_ref())
            .report_err(loc!())
    }

    #[inline] pub fn main_bar(&self) -> &NodeRef {
        &self.main_bar
    }

    #[inline] pub fn aux_bar(&self) -> &NodeRef {
        &self.aux_bar
    }
}
