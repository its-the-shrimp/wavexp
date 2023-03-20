#![allow(non_camel_case_types)] // because derive(yew::Properties) generates them

use std::f64::consts::{PI, TAU};
use wasm_bindgen::JsCast;
use yew::TargetCast;
use crate::draggable;
use crate::MainCmd;
use crate::utils;
use crate::utils::{JsResultUtils, HtmlCanvasExt, HtmlDocumentExt, OptionToJsResult};

pub struct Slider {
    value: f64,
    canvas: web_sys::HtmlCanvasElement,
    focused: bool,
    hovered: bool
}

#[derive(PartialEq, yew::Properties)]
pub struct SliderProps {
    pub name: &'static str,
    #[prop_or(1.0)]
    pub coef: f64,
    #[prop_or(2)]
    pub precision: usize,
    #[prop_or("")]
    pub postfix: &'static str,
    pub component_id: usize,
    pub id: usize,
    pub initial: f64
}

const LINE_WIDTH: f64 = 10.0;
const FONT: &'static str = "4em consolas";

impl yew::Component for Slider {
    type Message = draggable::Cmd;
    type Properties = SliderProps;

    fn create(ctx: &yew::Context<Self>) -> Self {
        let SliderProps {coef, initial, ..} = ctx.props();
        Self {focused: false, hovered: false, value: initial / coef,
            canvas: wasm_bindgen::JsValue::UNDEFINED.unchecked_into()}
    }

    fn update(&mut self, ctx: &yew::Context<Self>, msg: Self::Message) -> bool {
        _ = utils::js_try!{type = !:
            let SliderProps {component_id, id, coef, name, ..} = ctx.props();
            msg.handle_hover(name, |_, hovered| Ok(self.hovered = hovered))?
                .handle_focus(|_| Ok(self.focused = true))?
                .handle_unfocus(|_| Ok({
                    self.focused = false;
                    MainCmd::SetParam(*component_id, *id, self.value * coef).send();
                }))?
                .handle_drag(|e| Ok({
                    let target = e.target_dyn_into::<web_sys::Element>()
                        .to_js_result("no target on a pointer event")?;
                    self.value = (self.value + e.movement_y() as f64 / (target.client_height() * -2) as f64)
                        .clamp(0.0, 1.0);
                }))?;
            return true
        }.report_err("handling a message received by the slider");
        false
    }

	fn view(&self, ctx: &yew::Context<Self>) -> yew::Html {
        let SliderProps{component_id, id, .. } = ctx.props();
        yew::html! {
            <canvas id={format!("slider-{}-{}", *component_id, *id)} class="input"
            onpointerdown={ctx.link().callback(draggable::Cmd::Focus)}
            onpointerup={ctx.link().callback(draggable::Cmd::Unfocus)}
            onpointerenter={ctx.link().callback(draggable::Cmd::HoverIn)}
            onpointerleave={ctx.link().callback(draggable::Cmd::HoverOut)}
            onpointermove={self.focused.then(|| ctx.link().callback(draggable::Cmd::Drag))}/>}
    }

    fn rendered(&mut self, ctx: &yew::Context<Self>, first_render: bool) {
        _ = utils::js_try!{type = !:
            let SliderProps{coef, precision, postfix, component_id, id, ..} = ctx.props();
            if first_render {
                self.canvas = utils::document()
                    .element_dyn_into(&format!("slider-{}-{}", *component_id, *id))?;
                self.canvas.sync();
            }
            let ctx = self.canvas.get_2d_context()?;
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
                ctx.arc(w, h + LINE_WIDTH, r, 0.0, PI * 2.0)?;
                ctx.stroke();
                ctx.begin_path()}
            if self.value != 0.0 {
                ctx.arc(w, h + LINE_WIDTH, r - LINE_WIDTH,
                    PI * 1.5, (self.value * 2.0 + 1.5) * PI)?;
                ctx.stroke()}
            ctx.set_text_baseline("middle");
            ctx.fill_text_with_max_width(&format!("{:.*}", precision, coef * self.value),
                w, h + LINE_WIDTH / 2.0, r)?;
            ctx.set_text_baseline("top");
            return ctx.fill_text_with_max_width(postfix, 
                w, h + LINE_WIDTH * 2.0 + ctx.measure_text("0")?
                    .font_bounding_box_ascent() * 2.0, r * 1.5)?
        }.report_err("rendering the slider");
    }
}

pub struct Switch {
    value: f64,
    canvas: web_sys::HtmlCanvasElement,
    focused: bool,
    hovered: bool
}

#[derive(PartialEq, yew::Properties)]
pub struct SwitchProps {
    pub name: &'static str,
    pub options: Vec<&'static str>,
    pub component_id: usize,
    pub id: usize,
    pub initial: usize
}

impl yew::Component for Switch {
    type Message = draggable::Cmd;
    type Properties = SwitchProps;

    fn create(ctx: &yew::Context<Self>) -> Self {
        Self {focused: false, hovered: false, value: ctx.props().initial as f64,
            canvas: wasm_bindgen::JsValue::UNDEFINED.unchecked_into()}
    }

    fn update(&mut self, ctx: &yew::Context<Self>, msg: Self::Message) -> bool {
        _ = utils::js_try!{type = !:
            let SwitchProps {options, component_id, id, name, ..} = ctx.props();
            msg.handle_hover(name, |_, hovered| Ok(self.hovered = hovered))?
                .handle_focus(|_| Ok(self.focused = true))?
                .handle_unfocus(|_| Ok(self.focused = false))?
                .handle_drag(|e| {
                    let old_value = self.value as usize;
                    let target = e.target_dyn_into::<web_sys::Element>()
                        .to_js_result("no target on a pointer event")?;
                    self.value = (self.value + e.movement_y() as f64 / target.client_height() as f64 / -2.0 * options.len() as f64)
                        .rem_euclid(options.len() as f64);
                    if old_value != self.value as usize {
                        MainCmd::SetParam(*component_id, *id, self.value.floor()).send()}
                    Ok(())})?;
            return true
        }.report_err("handling a message received by the slider");
        false
    }

	fn view(&self, ctx: &yew::Context<Self>) -> yew::Html {
        let SwitchProps{component_id, id, .. } = ctx.props();
        yew::html! {
            <canvas id={format!("switch-{}-{}", *component_id, *id)} class="input"
            onpointerdown={ctx.link().callback(draggable::Cmd::Focus)}
            onpointerup={ctx.link().callback(draggable::Cmd::Unfocus)}
            onpointerenter={ctx.link().callback(draggable::Cmd::HoverIn)}
            onpointerleave={ctx.link().callback(draggable::Cmd::HoverOut)}
            onpointermove={self.focused.then(|| ctx.link().callback(draggable::Cmd::Drag))}/>}
    }

    fn rendered(&mut self, ctx: &yew::Context<Self>, first_render: bool) {
        _ = utils::js_try!{
            let SwitchProps{options, component_id, id, ..} = ctx.props();
            if first_render {
                self.canvas = utils::document()
                    .element_dyn_into(&format!("switch-{}-{}", *component_id, *id))?;
                self.canvas.sync();
            }
            let ctx = self.canvas.get_2d_context()?;
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
                ctx.arc(w, h + LINE_WIDTH, r, 0.0, PI * 2.0)?;
                ctx.stroke();
                ctx.begin_path()}
            let (factor, index) = (options.len() as f64 / TAU, self.value.floor());
            ctx.arc(w, h + LINE_WIDTH, r - LINE_WIDTH, index / factor, (index + 1.0) / factor)?;
            ctx.stroke();
            return ctx.fill_text_with_max_width(unsafe{options.get_unchecked(index as usize)},
                w, h + LINE_WIDTH, w)?;
        }.report_err("rendering the switch");
    }
}

pub struct Button;

#[derive(PartialEq, yew::Properties)]
pub struct ButtonProps {
    #[prop_or_default]
    pub style: &'static str,
    pub desc: &'static str,
    pub children: yew::html::Children,
    pub component_id: usize,
    pub id: usize
}

impl yew::Component for Button {
    type Message = draggable::Cmd;
    type Properties = ButtonProps;

    fn create(_: &yew::Context<Self>) -> Self {Self}

    fn update(&mut self, ctx: &yew::Context<Self>, msg: Self::Message) -> bool {
        _ = utils::js_try!{
            let ButtonProps{desc, component_id, id, ..} = ctx.props();
            msg.handle_hover(desc, |_, _| Ok(()))?
                .handle_focus(|_| Ok(MainCmd::SetParam(*component_id, *id, f64::INFINITY).send()))?
                .handle_unfocus(|_| Ok(MainCmd::SetParam(*component_id, *id, f64::NEG_INFINITY).send()))?;
        }.report_err("handling a message received by the button");
        false
    }

    fn view(&self, ctx: &yew::Context<Self>) -> yew::Html {
        let ButtonProps{style, children, ..} = ctx.props();
        yew::html!{
            <div class="input button" style={*style}
            onpointerdown={ctx.link().callback(draggable::Cmd::Focus)}
            onpointerup={ctx.link().callback(draggable::Cmd::Unfocus)}
            onpointerenter={ctx.link().callback(draggable::Cmd::HoverIn)}
            onpointerleave={ctx.link().callback(draggable::Cmd::HoverOut)}>{children.clone()}</div>
        }
    }
}
