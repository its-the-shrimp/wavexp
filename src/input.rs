use std::{
    f64::consts::{PI, TAU},
    ops::{Div, Mul, Add, Deref, DerefMut}};
use wasm_bindgen::JsValue;
use web_sys::{
    Element,
    HtmlCanvasElement,
    PointerEvent,
    HtmlElement,
    MouseEvent,
    KeyboardEvent};
use yew::{
    html, 
    Component,
    Context,
    Html,
    TargetCast,
    html::Children,
    Classes,
    AttrValue,
    NodeRef,
    Callback};
use crate::{
    utils::{js_try, JsResultUtils, HtmlCanvasExt, OptionExt, BoolExt, R64, Point, HtmlElementExt},
    loc};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Buttons {
    pub left: bool,
    pub shift: bool,
    pub meta: bool
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Cursor {
    pub point: Point,
    buttons: Buttons
}

impl Deref for Cursor {
    type Target = Buttons;
    #[inline] fn deref(&self) -> &Self::Target {&self.buttons}
}

impl DerefMut for Cursor {
    #[inline] fn deref_mut(&mut self) -> &mut Self::Target {&mut self.buttons}
}

impl Add<&KeyboardEvent> for Cursor {
    type Output = Self;
    #[inline] fn add(mut self, rhs: &KeyboardEvent) -> Self::Output {
        self.shift = rhs.shift_key();
        self.meta = rhs.meta_key();
        self
    }
}

impl TryFrom<&MouseEvent> for Cursor {
    type Error = JsValue;
    fn try_from(value: &MouseEvent) -> Result<Self, Self::Error> {
        let canvas: HtmlCanvasElement = value.target_dyn_into().to_js_result(loc!())?;
        let point = Point{x: value.offset_x(), y: value.offset_y()}
            .normalise(canvas.client_rect(), canvas.rect());
        Ok(Self{point, buttons: Buttons{
            left: value.buttons() & 1 == 1,
            shift: value.shift_key(),
            meta: value.meta_key()}})
    }
}

impl TryFrom<&PointerEvent> for Cursor {
    type Error = JsValue;
    fn try_from(value: &PointerEvent) -> Result<Self, Self::Error> {
        let canvas: HtmlCanvasElement = value.target_dyn_into().to_js_result(loc!())?;
        let point = Point{x: value.offset_x(), y: value.offset_y()}
            .normalise(canvas.client_rect(), canvas.rect());
        Ok(Self{point, buttons: Buttons{
            left: value.buttons() & 1 == 1,
            shift: value.shift_key(),
            meta: value.meta_key()}})
    }
}

#[derive(Debug)]
pub enum Cmd {
    Drag(PointerEvent),
    Focus(PointerEvent),
    Unfocus(PointerEvent),
    HoverIn(PointerEvent),
    HoverOut(PointerEvent)
}

pub struct Slider {
    value: R64,
    canvas: NodeRef,
    focused: bool,
    hovered: bool,
    floored: bool
}

#[derive(PartialEq, yew::Properties)]
pub struct SliderProps {
    pub name: AttrValue,
    #[prop_or(false)]
    pub signed: bool,
    #[prop_or(R64::ONE)]
    pub max: R64,
    #[prop_or(R64::ZERO)]
    pub min: R64,
    #[prop_or(2)]
    pub precision: usize,
    #[prop_or("")]
    pub postfix: &'static str,
    pub setter: Callback<R64>,
    pub initial: R64
}

const LINE_WIDTH: f64 = 10.0;
const FONT: &str = "4em consolas";

impl Component for Slider {
    type Message = Cmd;
    type Properties = SliderProps;

    fn create(ctx: &Context<Self>) -> Self {
        Self {focused: false, hovered: false, floored: false,
            value: ctx.props().initial, canvas: NodeRef::default()}
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        _ = js_try!{type = !:
            let SliderProps{setter, min, max, signed, ..} = ctx.props();
            match msg {
                Cmd::Drag(e) => {
                    let target: Element = e.target_dyn_into().to_js_result(loc!())?;
                    self.value = R64::from(e.movement_y())
                        .div(target.client_height() * -2)
                        .mul(max - min)
                        .add(self.value)
                        .clamp(signed.choose(-*max, *min), *max);
                    self.floored = e.shift_key();
                }

                Cmd::Focus(e) => {
                    e.target_dyn_into::<HtmlElement>().to_js_result(loc!())?
                        .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    self.focused = true;
                    self.floored = e.shift_key();
                }

                Cmd::Unfocus(e) => {
                    e.target_dyn_into::<HtmlElement>().to_js_result(loc!())?
                        .release_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    self.focused = false;
                    self.floored = e.shift_key();
                    setter.emit(if self.floored {self.value.floor()} else {self.value});
                }

                Cmd::HoverIn(_) => self.hovered = true,

                Cmd::HoverOut(_) => self.hovered = false,
            }
            return true
        }.report_err(loc!());
        false
    }

	fn view(&self, ctx: &Context<Self>) -> Html {
        html! {
            <canvas ref={self.canvas.clone()} class="input"
            data-main-hint={&ctx.props().name}
            onpointerdown={ctx.link().callback(Cmd::Focus)}
            onpointerup={ctx.link().callback(Cmd::Unfocus)}
            onpointerover={ctx.link().callback(Cmd::HoverIn)}
            onpointerleave={ctx.link().callback(Cmd::HoverOut)}
            onpointermove={self.focused.then(|| ctx.link().callback(Cmd::Drag))}/>}
    }

    fn rendered(&mut self, ctx: &Context<Self>, first_render: bool) {
        _ = js_try!{type = !:
            let SliderProps{min, max, precision, postfix, ..} = ctx.props();
            let canvas: HtmlCanvasElement = self.canvas.cast().to_js_result(loc!())?;
            if first_render {canvas.sync()}
            let ctx = canvas.get_2d_context(loc!())?;
            let (w, h) = (canvas.width() as f64 / 2.0, canvas.height() as f64 / 2.0);
            const CORRECTION_COEF: f64 = 1.5;
            let r = w.min(h) - LINE_WIDTH * CORRECTION_COEF; 
            if first_render {
                ctx.set_fill_style(&"#0069E1".into());
                ctx.set_stroke_style(&"#0069E1".into());
                ctx.set_font(FONT);
                ctx.set_line_width(LINE_WIDTH);
                ctx.set_text_align("center")}
            ctx.clear_rect(0.0, 0.0, w * 2.0, h * 2.0);
            ctx.begin_path();
            if self.hovered {
                ctx.arc(w, h + LINE_WIDTH, r, 0.0, PI * 2.0).add_loc(loc!())?;
                ctx.stroke();
                ctx.begin_path()}
            let value = if self.floored {self.value.floor()} else {self.value};
            if value != *min {
                ctx.arc_with_anticlockwise(w, h + LINE_WIDTH, r - LINE_WIDTH,
                    PI * 1.5,
                    ((*value - **min) / (**max - **min) * 2.0 + 1.5) * PI,
                    self.value < *min).add_loc(loc!())?;
                ctx.stroke();
            }
            ctx.set_text_baseline("middle");
            ctx.fill_text_with_max_width(&format!("{:.*}", precision, *value),
                w, h + LINE_WIDTH / 2.0, r).add_loc(loc!())?;
            ctx.set_text_baseline("top");
            return ctx.fill_text_with_max_width(postfix, 
                w, h + LINE_WIDTH * 2.0 + ctx.measure_text("0").add_loc(loc!())?
                    .font_bounding_box_ascent() * 2.0, r * 1.5).add_loc(loc!())?
        }.report_err(loc!());
    }
}

pub struct Switch {
    value: R64,
    canvas: NodeRef,
    focused: bool,
    hovered: bool
}

#[derive(PartialEq, yew::Properties)]
pub struct SwitchProps {
    pub name: AttrValue,
    pub options: Vec<&'static str>,
    pub setter: Callback<usize>,
    pub initial: usize
}

impl Component for Switch {
    type Message = Cmd;
    type Properties = SwitchProps;

    fn create(ctx: &Context<Self>) -> Self {
        Self {focused: false, hovered: false,
            value: R64::from(ctx.props().initial), canvas: NodeRef::default()}
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        _ = js_try!{type = !:
            let SwitchProps{options, setter, ..} = ctx.props();
            match msg {
                Cmd::Drag(e) => {
                    let old_value = *self.value as usize;
                    let h = e.target_dyn_into::<Element>().to_js_result(loc!())?.client_height();
                    self.value = (self.value + R64::from(e.movement_y()) / h / -4i8 * options.len())
                        .rem_euclid(options.len().into()).to_js_result(loc!())?;
                    if old_value != *self.value as usize {
                        setter.emit(*self.value as usize)
                    }
                }

                Cmd::Focus(e) => {
                    e.target_dyn_into::<HtmlElement>().to_js_result(loc!())?
                        .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    self.focused = true;
                }

                Cmd::Unfocus(e) => {
                    e.target_dyn_into::<HtmlElement>().to_js_result(loc!())?
                        .release_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    self.focused = false;
                }

                Cmd::HoverIn(_) => self.hovered = true,

                Cmd::HoverOut(_) => self.hovered = false,
            }
            return true
        }.report_err(loc!());
        false
    }

	fn view(&self, ctx: &Context<Self>) -> Html {
        html! {
            <canvas ref={self.canvas.clone()} class="input"
            data-main-hint={&ctx.props().name}
            onpointerdown={ctx.link().callback(Cmd::Focus)}
            onpointerup={ctx.link().callback(Cmd::Unfocus)}
            onpointerenter={ctx.link().callback(Cmd::HoverIn)}
            onpointerleave={ctx.link().callback(Cmd::HoverOut)}
            onpointermove={self.focused.then(|| ctx.link().callback(Cmd::Drag))}/>}
    }

    fn rendered(&mut self, ctx: &Context<Self>, first_render: bool) {
        _ = js_try!{
            let options = &ctx.props().options;
            let canvas: HtmlCanvasElement = self.canvas.cast().to_js_result(loc!())?;
            if first_render {canvas.sync()}
            let ctx = canvas.get_2d_context(loc!())?;
            let (w, h) = (canvas.width() as f64 / 2.0, canvas.height() as f64 / 2.0);
            const CORRECTION_COEF: f64 = 1.5;
            let r = w.min(h) - LINE_WIDTH * CORRECTION_COEF;
            if first_render {
                ctx.set_fill_style(&"#0069E1".into());
                ctx.set_stroke_style(&"#0069E1".into());
                ctx.set_line_width(LINE_WIDTH);
                ctx.set_font(FONT);
                ctx.set_text_align("center");
                ctx.set_text_baseline("middle")}
            ctx.clear_rect(0.0, 0.0, w * 2.0, h * 2.0);
            ctx.begin_path();
            if self.hovered {
                ctx.arc(w, h + LINE_WIDTH, r, 0.0, PI * 2.0).add_loc(loc!())?;
                ctx.stroke();
                ctx.begin_path()}
            let (factor, index) = (options.len() as f64 / TAU, *self.value.floor());
            ctx.arc(w, h + LINE_WIDTH, r - LINE_WIDTH, index / factor, (index + 1.0) / factor).add_loc(loc!())?;
            ctx.stroke();
            return ctx.fill_text_with_max_width(unsafe{options.get_unchecked(index as usize)},
                w, h + LINE_WIDTH, w).add_loc(loc!())?;
        }.report_err(loc!());
    }
}

pub struct Button;

#[derive(PartialEq, yew::Properties)]
pub struct ButtonProps {
    pub name: AttrValue,
    pub children: Children,
    pub setter: Callback<()>,
    #[prop_or(false)]
    pub svg: bool,
    #[prop_or_default]
    pub class: Classes,
    #[prop_or_default]
    pub help: AttrValue
}

impl Component for Button {
    type Message = ();
    type Properties = ButtonProps;

    #[inline] fn create(_: &Context<Self>) -> Self {Self}

    #[inline]
    fn update(&mut self, _: &Context<Self>, _: Self::Message) -> bool {false}

    fn view(&self, ctx: &Context<Self>) -> Html {
        let ButtonProps{name, children, svg, class, setter, help} = ctx.props();
        let mut class = class.clone();
        class.push("input button");
        if *svg {
            html!{
                <g {class}
                data-main-hint={name} data-aux-hint={help}
                onpointerup={setter.reform(|_| ())}>
                    {children.clone()}
                </g>
            }
        } else {
            html!{
                <button {class}
                data-main-hint={name} data-aux-hint={help}
                onpointerup={setter.reform(|_| ())}>
                    {children.clone()}
                </button>
            }
        }
    }
}
