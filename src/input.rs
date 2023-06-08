#![allow(non_camel_case_types)] // because derive(yew::Properties) generates them

use std::{
    f64::consts::{PI, TAU},
    ops::{Div, Mul, Add},
    rc::Rc};
use wasm_bindgen::{JsCast, JsValue};
use web_sys::{Element, HtmlCanvasElement, PointerEvent, HtmlElement};
use yew::{
    html, 
    Component,
    Context,
    Html,
    TargetCast,
    html::Children, Classes, AttrValue};
use crate::{
    utils::{
        document,
        js_try,
        JsResultUtils,
        HtmlCanvasExt,
        HtmlDocumentExt,
        OptionExt, BoolExt, R64},
    sound::SoundType,
    visual::HintHandler,
    MainCmd,
    loc};

#[derive(Debug)]
pub enum Cmd {
    Drag(PointerEvent),
    Focus(PointerEvent),
    Unfocus(PointerEvent),
    HoverIn(PointerEvent),
    HoverOut(PointerEvent)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ParamId {
    Play,
    Select(Option<usize>),
    Add(SoundType, i32),
    Remove(usize),
    Note(usize),
    NoteLength(usize),
    Bpm,
    MasterGain,
    SnapStep
}

impl ParamId {
    /// returns `Some(id)` when the parameter ID belongs to a specific sound block
    pub fn block_id(&self) -> Option<usize> {
        match self {
            ParamId::Play
            | ParamId::Select(..)
            | ParamId::Add(..)
            | ParamId::Remove(..)
            | ParamId::Bpm
            | ParamId::MasterGain
            | ParamId::SnapStep => None,

            ParamId::Note(id)
            | ParamId::NoteLength(id) => Some(*id)
        }
    }

    /// returns `true` if the editor plane needs to be re-rendered
    /// after setting the parameter
    pub fn need_plane_rerender(&self) -> bool {
        match self {
            ParamId::Select(..)
            | ParamId::Add(..)
            | ParamId::Remove(..)
            | ParamId::NoteLength(..) => true,

            ParamId::Note(..)
            | ParamId::Play
            | ParamId::Bpm
            | ParamId::MasterGain
            | ParamId::SnapStep => false,
        }
    }
}

pub struct Slider {
    value: R64,
    canvas: HtmlCanvasElement,
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
    pub id: ParamId,
    pub initial: R64,
    pub hint: Rc<HintHandler>
}

const LINE_WIDTH: f64 = 10.0;
const FONT: &str = "4em consolas";

impl Component for Slider {
    type Message = Cmd;
    type Properties = SliderProps;

    fn create(ctx: &Context<Self>) -> Self {
        Self {focused: false, hovered: false, floored: false,
            value: ctx.props().initial, canvas: JsValue::UNDEFINED.unchecked_into()}
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        _ = js_try!{type = !:
            let SliderProps{id, min, max, name, signed, hint, ..} = ctx.props();
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
                    MainCmd::SetParam(*id,
                        if self.floored {self.value.floor()} else {self.value}).send();
                }

                Cmd::HoverIn(_) => {
                    self.hovered = true;
                    hint.set_hint(name, "").add_loc(loc!())?;
                }

                Cmd::HoverOut(_) => self.hovered = false,
            }
            return true
        }.report_err(loc!());
        false
    }

	fn view(&self, ctx: &Context<Self>) -> Html {
        let SliderProps{id, .. } = ctx.props();
        html! {
            <canvas id={format!("Slider({:?})", *id)} class="input"
            onpointerdown={ctx.link().callback(Cmd::Focus)}
            onpointerup={ctx.link().callback(Cmd::Unfocus)}
            onpointerenter={ctx.link().callback(Cmd::HoverIn)}
            onpointerleave={ctx.link().callback(Cmd::HoverOut)}
            onpointermove={self.focused.then(|| ctx.link().callback(Cmd::Drag))}/>}
    }

    fn rendered(&mut self, ctx: &Context<Self>, first_render: bool) {
        _ = js_try!{type = !:
            let SliderProps{min, max, precision, postfix, id, ..} = ctx.props();
            if first_render {
                self.canvas = document()
                    .element_dyn_into(&format!("Slider({:?})", *id), loc!())?;
                self.canvas.sync();
            }
            let ctx = self.canvas.get_2d_context(loc!())?;
            let (w, h) = (self.canvas.width() as f64 / 2.0, self.canvas.height() as f64 / 2.0);
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
    canvas: HtmlCanvasElement,
    focused: bool,
    hovered: bool
}

#[derive(PartialEq, yew::Properties)]
pub struct SwitchProps {
    pub name: AttrValue,
    pub options: Vec<&'static str>,
    pub id: ParamId,
    pub initial: usize,
    pub hint: Rc<HintHandler>
}

impl Component for Switch {
    type Message = Cmd;
    type Properties = SwitchProps;

    fn create(ctx: &Context<Self>) -> Self {
        Self {focused: false, hovered: false, value: R64::from(ctx.props().initial),
            canvas: JsValue::UNDEFINED.unchecked_into()}
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        _ = js_try!{type = !:
            let SwitchProps {options, id, name, hint, ..} = ctx.props();
            match msg {
                Cmd::Drag(e) => {
                    let old_value = *self.value as usize;
                    let h = e.target_dyn_into::<Element>().to_js_result(loc!())?.client_height();
                    self.value = (self.value + R64::from(e.movement_y()) / h / -4i8 * options.len())
                        .rem_euclid(options.len().into()).to_js_result(loc!())?;
                    if old_value != *self.value as usize {
                        MainCmd::SetParam(*id, self.value.floor()).send()
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

                Cmd::HoverIn(_) => {
                    self.hovered = true;
                    hint.set_hint(name, "").add_loc(loc!())?;
                }

                Cmd::HoverOut(_) => self.hovered = false,
            }
            return true
        }.report_err(loc!());
        false
    }

	fn view(&self, ctx: &Context<Self>) -> Html {
        html! {
            <canvas id={format!("Switch({:?})", ctx.props().id)} class="input"
            onpointerdown={ctx.link().callback(Cmd::Focus)}
            onpointerup={ctx.link().callback(Cmd::Unfocus)}
            onpointerenter={ctx.link().callback(Cmd::HoverIn)}
            onpointerleave={ctx.link().callback(Cmd::HoverOut)}
            onpointermove={self.focused.then(|| ctx.link().callback(Cmd::Drag))}/>}
    }

    fn rendered(&mut self, ctx: &Context<Self>, first_render: bool) {
        _ = js_try!{
            let SwitchProps{options, id, ..} = ctx.props();
            if first_render {
                self.canvas = document()
                    .element_dyn_into(&format!("Switch({:?})", *id), loc!())?;
                self.canvas.sync();
            }
            let ctx = self.canvas.get_2d_context(loc!())?;
            let (w, h) = (self.canvas.width() as f64 / 2.0, self.canvas.height() as f64 / 2.0);
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
    pub id: ParamId,
    pub hint: Rc<HintHandler>,
    #[prop_or(false)]
    pub svg: bool,
    #[prop_or_default]
    pub class: Classes
}

impl Component for Button {
    type Message = Cmd;
    type Properties = ButtonProps;

    fn create(_: &Context<Self>) -> Self {Self}

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        _ = js_try!{
            let ButtonProps{name, id, hint, ..} = ctx.props();
            match msg {
                Cmd::Drag(_) => (),
                Cmd::Focus(_) => MainCmd::SetParam(*id, R64::INFINITY).send(),
                Cmd::Unfocus(_) => MainCmd::SetParam(*id, R64::NEG_INFINITY).send(),
                Cmd::HoverIn(_) => hint.set_hint(name, "").add_loc(loc!())?,
                Cmd::HoverOut(_) => ()
            }
        }.report_err(loc!());
        false
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let mut class = ctx.props().class.clone();
        class.push("input button");
        if ctx.props().svg {
            html!{
                <g {class}
                onpointerdown={ctx.link().callback(Cmd::Focus)}
                onpointerup={ctx.link().callback(Cmd::Unfocus)}
                onpointerenter={ctx.link().callback(Cmd::HoverIn)}
                onpointerleave={ctx.link().callback(Cmd::HoverOut)}>
                    {ctx.props().children.clone()}
                </g>
            }
        } else {
            html!{
                <button {class} id={format!("Button({:?})", ctx.props().id)}
                onpointerdown={ctx.link().callback(Cmd::Focus)}
                onpointerup={ctx.link().callback(Cmd::Unfocus)}
                onpointerenter={ctx.link().callback(Cmd::HoverIn)}
                onpointerleave={ctx.link().callback(Cmd::HoverOut)}>
                    {ctx.props().children.clone()}
                </button>
            }
        }
    }
}
