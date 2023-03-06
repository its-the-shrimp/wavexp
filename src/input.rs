use std::f64::consts::PI;
use wasm_bindgen::JsCast;
use yew::TargetCast;
use crate::draggable;
use crate::MainCmd;
use crate::utils::HtmlCanvasExt;
use crate::utils::HtmlDocumentExt;
use crate::utils::OptionToJsResult;
use crate::utils::{self, JsResultUtils};

pub struct Slider {
    value: f64,
    canvas: web_sys::HtmlCanvasElement,
    focused: bool
}

#[derive(PartialEq, yew::Properties)]
pub struct SliderProps {
    #[prop_or(yew::classes!("default-input"))]
    pub class: yew::html::Classes,
    pub name: &'static str,
    #[prop_or(1.0)]
    pub coef: f64,
    #[prop_or(2)]
    pub precision: usize,
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
        Self {focused: false, value: initial / coef,
            canvas: wasm_bindgen::JsValue::UNDEFINED.unchecked_into()}
    }

    fn update(&mut self, ctx: &yew::Context<Self>, msg: Self::Message) -> bool {
        _ = utils::js_try!{type = !:
            let SliderProps {component_id, id, coef, name, ..} = ctx.props();
            return msg.handle_hover(name)
                .handle_focus(|_| Ok(self.focused = true))?
                .handle_unfocus(|_| Ok(self.focused = false))?
                .handle_drag(|e| {
                    let target = e.target_dyn_into::<web_sys::Element>()
                        .to_js_result("no target on a pointer event")?;
                    self.value = (self.value + e.movement_y() as f64 / (target.client_height() * -2) as f64)
                        .clamp(0.0, 1.0);
                    MainCmd::SetParam(*component_id, *id, self.value * coef).send();
                    Ok(())})?
                .needs_rerender()
        }.report_err("handling a message received by the slider");
        false
    }

	fn view(&self, ctx: &yew::Context<Self>) -> yew::Html {
        let SliderProps { class, component_id, id, .. } = ctx.props();
        yew::html! {
            <canvas id={format!("slider-{}-{}", *component_id, *id)} class={class.clone()}
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
                ctx.set_line_width(LINE_WIDTH);
                ctx.set_font(FONT);
                ctx.set_text_align("center")}
            ctx.clear_rect(0.0, 0.0, w * 2.0, h * 2.0);
            ctx.begin_path();
            if self.value != 0.0 {
                ctx.arc(w, h + LINE_WIDTH, r,
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
    focused: bool
}

#[derive(PartialEq, yew::Properties)]
pub struct SwitchProps {
    #[prop_or(yew::classes!("default-input"))]
    pub class: yew::Classes,
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
        Self {focused: false, value: ctx.props().initial as f64,
            canvas: wasm_bindgen::JsValue::UNDEFINED.unchecked_into()}
    }

    fn update(&mut self, ctx: &yew::Context<Self>, msg: Self::Message) -> bool {
        let err: utils::JsResult<!> = try {
            let SwitchProps {options, component_id, id, name, ..} = ctx.props();
            return msg.handle_hover(name)
                .handle_focus(|_| Ok(self.focused = true))?
                .handle_unfocus(|_| Ok(self.focused = false))?
                .handle_drag(|e| {
                    let old_value = self.value as usize;
                    let target = e.target_dyn_into::<web_sys::Element>()
                        .to_js_result("no target on a pointer event")?;
                    self.value = (self.value + e.movement_y() as f64 / target.client_height() as f64 / -0.5)
                        .rem_euclid(options.len() as f64);
                    if old_value != self.value as usize {
                        MainCmd::SetParam(*component_id, *id, self.value).send()}
                    Ok(())})?
                .needs_rerender()
        };
        _ = err.report_err("handling a message received by the slider");
        false
    }

	fn view(&self, ctx: &yew::Context<Self>) -> yew::Html {
        let SwitchProps { class, component_id, id, .. } = ctx.props();
        yew::html! {
            <canvas id={format!("switch-{}-{}", *component_id, *id)} class={class.clone()}
            onpointerdown={ctx.link().callback(draggable::Cmd::Focus)}
            onpointerup={ctx.link().callback(draggable::Cmd::Unfocus)}
            onpointerenter={ctx.link().callback(draggable::Cmd::HoverIn)}
            onpointerleave={ctx.link().callback(draggable::Cmd::HoverOut)}
            onpointermove={self.focused.then(|| ctx.link().callback(draggable::Cmd::Drag))}/>}
    }

    fn rendered(&mut self, ctx: &yew::Context<Self>, first_render: bool) {
        let err: utils::JsResult<()> = try {
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
            let index = self.value.floor();
            ctx.begin_path();
            ctx.arc(w, h + LINE_WIDTH, r, (index / 2.0 + 1.5) * PI, index / 2.0 * PI)?;
            ctx.stroke();
            return ctx.fill_text_with_max_width(unsafe{options.get_unchecked(index as usize)},
                w, h + LINE_WIDTH, r * 1.5)?;
        };
        _ = err.report_err("rendering the switch");
    }
}
