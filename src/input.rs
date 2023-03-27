#![allow(non_camel_case_types)] // because derive(yew::Properties) generates them

use std::f64::consts::{PI, TAU};
use wasm_bindgen::{JsCast, JsValue};
use web_sys::{Element, HtmlCanvasElement, PointerEvent, HtmlElement};
use yew::{html, 
    Component,
    Context,
    Html,
    TargetCast,
    html::Children};
use crate::{
    utils::{
        document,
        js_try,
        JsResultUtils,
        HtmlCanvasExt,
        HtmlDocumentExt,
        OptionToJsResult, JsResult},
    MainCmd, loc};

#[derive(Debug)]
pub enum Cmd {
    Drag(PointerEvent),
    Focus(PointerEvent),
    Unfocus(PointerEvent),
    HoverIn(PointerEvent),
    HoverOut(PointerEvent)
}

impl Cmd {
    pub fn handle_focus<F>(self, f: F) -> JsResult<Self>
    where F: FnOnce(&PointerEvent) -> JsResult<()> {
        if let Cmd::Focus(e) = &self {
            e.target_unchecked_into::<HtmlElement>()
                .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
            f(e).add_loc(loc!())?;
        }
        Ok(self)
    }

    pub fn handle_unfocus<F>(self, f: F) -> JsResult<Self>
    where F: FnOnce(&PointerEvent) -> JsResult<()> {
        if let Cmd::Unfocus(e) = &self {
            e.target_unchecked_into::<HtmlElement>()
                .release_pointer_capture(e.pointer_id()).add_loc(loc!())?;
            f(e).add_loc(loc!())?;
        }
        Ok(self)
    }

    /// the boolean passed to `f` indicates whether the element is hovered over
    #[inline]
    pub fn handle_hover<F>(self, help_msg: &str, f: F) -> JsResult<Self>
    where F: FnOnce(&PointerEvent, bool) -> JsResult<()> {
        match &self {
            Cmd::HoverIn(e) => {
                f(e, true).add_loc(loc!())?;
                MainCmd::SetDesc(help_msg.to_owned()).send()}
            Cmd::HoverOut(e) => {
                f(e, false).add_loc(loc!())?;
                MainCmd::RemoveDesc.send()}
            _ => ()};
        Ok(self)
    }

    pub fn handle_drag(self, f: impl FnOnce(&PointerEvent) -> JsResult<()>) -> JsResult<Self> {
        if let Cmd::Drag(e) = &self {
            f(e).add_loc(loc!())?;
        };
        Ok(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ParamId {
    Play,
    Disconnect(usize),
    Remove(usize),
    WaveFreq(usize, u32),
    WaveType(usize, u32),
    RemoveWave(usize, u32),
    AddWave(usize),
    EnvelopeAttack(usize),
    EnvelopeDecay(usize),
    EnvelopSustain(usize),
    EnvelopeRelease(usize)
}

impl ParamId {
    // returns Some(id) when the parameter ID belongs to a specific element
    pub fn element_id(&self) -> Option<usize> {
        match self {
            &ParamId::Play                => None,
            &ParamId::Disconnect(id)      => Some(id),
            &ParamId::Remove(id)          => Some(id),
            &ParamId::WaveFreq(id, _)     => Some(id),
            &ParamId::WaveType(id, _)     => Some(id),
            &ParamId::RemoveWave(id, _)   => Some(id),
            &ParamId::AddWave(id)         => Some(id),
            &ParamId::EnvelopeAttack(id)  => Some(id),
            &ParamId::EnvelopeDecay(id)   => Some(id),
            &ParamId::EnvelopSustain(id)  => Some(id),
            &ParamId::EnvelopeRelease(id) => Some(id),
        }
    }
}

pub struct Slider {
    value: f64,
    canvas: HtmlCanvasElement,
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
    pub id: ParamId,
    pub initial: f64
}

const LINE_WIDTH: f64 = 10.0;
const FONT: &'static str = "4em consolas";

impl Component for Slider {
    type Message = Cmd;
    type Properties = SliderProps;

    fn create(ctx: &Context<Self>) -> Self {
        let SliderProps {coef, initial, ..} = ctx.props();
        Self {focused: false, hovered: false, value: initial / coef,
            canvas: JsValue::UNDEFINED.unchecked_into()}
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        _ = js_try!{type = !:
            let SliderProps{id, coef, name, ..} = ctx.props();
            msg.handle_hover(name, |_, hovered| Ok(self.hovered = hovered)).add_loc(loc!())?
                .handle_focus(|_| Ok(self.focused = true)).add_loc(loc!())?
                .handle_unfocus(|_| Ok({
                    self.focused = false;
                    MainCmd::SetParam(*id, self.value * coef).send();
                })).add_loc(loc!())?
                .handle_drag(|e| Ok({
                    let target = e.target_dyn_into::<Element>()
                        .to_js_result("no target on a pointer event").add_loc(loc!())?;
                    self.value = (self.value + e.movement_y() as f64 / (target.client_height() * -2) as f64)
                        .clamp(0.0, 1.0);
                })).add_loc(loc!())?;
            return true
        }.report_err();
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
            let SliderProps{coef, precision, postfix, id, ..} = ctx.props();
            if first_render {
                self.canvas = document()
                    .element_dyn_into(&format!("Slider({:?})", *id)).add_loc(loc!())?;
                self.canvas.sync();
            }
            let ctx = self.canvas.get_2d_context().add_loc(loc!())?;
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
            if self.value != 0.0 {
                ctx.arc(w, h + LINE_WIDTH, r - LINE_WIDTH,
                    PI * 1.5, (self.value * 2.0 + 1.5) * PI).add_loc(loc!())?;
                ctx.stroke()}
            ctx.set_text_baseline("middle");
            ctx.fill_text_with_max_width(&format!("{:.*}", precision, coef * self.value),
                w, h + LINE_WIDTH / 2.0, r).add_loc(loc!())?;
            ctx.set_text_baseline("top");
            return ctx.fill_text_with_max_width(postfix, 
                w, h + LINE_WIDTH * 2.0 + ctx.measure_text("0").add_loc(loc!())?
                    .font_bounding_box_ascent() * 2.0, r * 1.5).add_loc(loc!())?
        }.report_err();
    }
}

pub struct Switch {
    value: f64,
    canvas: HtmlCanvasElement,
    focused: bool,
    hovered: bool
}

#[derive(PartialEq, yew::Properties)]
pub struct SwitchProps {
    pub name: &'static str,
    pub options: Vec<&'static str>,
    pub id: ParamId,
    pub initial: usize
}

impl Component for Switch {
    type Message = Cmd;
    type Properties = SwitchProps;

    fn create(ctx: &Context<Self>) -> Self {
        Self {focused: false, hovered: false, value: ctx.props().initial as f64,
            canvas: JsValue::UNDEFINED.unchecked_into()}
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        _ = js_try!{type = !:
            let SwitchProps {options, id, name, ..} = ctx.props();
            msg.handle_hover(name, |_, hovered| Ok(self.hovered = hovered)).add_loc(loc!())?
                .handle_focus(|_| Ok(self.focused = true)).add_loc(loc!())?
                .handle_unfocus(|_| Ok(self.focused = false)).add_loc(loc!())?
                .handle_drag(|e| {
                    let old_value = self.value as usize;
                    let target = e.target_dyn_into::<Element>()
                        .to_js_result("no target on a pointer event").add_loc(loc!())?;
                    self.value = (self.value + e.movement_y() as f64 / target.client_height() as f64 / -2.0 * options.len() as f64)
                        .rem_euclid(options.len() as f64);
                    if old_value != self.value as usize {
                        MainCmd::SetParam(*id, self.value.floor()).send()}
                    Ok(())}).add_loc(loc!())?;
            return true
        }.report_err();
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
                    .element_dyn_into(&format!("Switch({:?})", *id)).add_loc(loc!())?;
                self.canvas.sync();
            }
            let ctx = self.canvas.get_2d_context().add_loc(loc!())?;
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
            let (factor, index) = (options.len() as f64 / TAU, self.value.floor());
            ctx.arc(w, h + LINE_WIDTH, r - LINE_WIDTH, index / factor, (index + 1.0) / factor).add_loc(loc!())?;
            ctx.stroke();
            return ctx.fill_text_with_max_width(unsafe{options.get_unchecked(index as usize)},
                w, h + LINE_WIDTH, w).add_loc(loc!())?;
        }.report_err();
    }
}

pub struct Button;

#[derive(PartialEq, yew::Properties)]
pub struct ButtonProps {
    #[prop_or_default]
    pub style: &'static str,
    pub desc: &'static str,
    pub children: Children,
    pub id: ParamId
}

impl Component for Button {
    type Message = Cmd;
    type Properties = ButtonProps;

    fn create(_: &Context<Self>) -> Self {Self}

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        _ = js_try!{
            let ButtonProps{desc, id, ..} = ctx.props();
            msg.handle_hover(desc, |_, _| Ok(())).add_loc(loc!())?
                .handle_focus(|_| Ok(MainCmd::SetParam(*id, f64::INFINITY).send())).add_loc(loc!())?
                .handle_unfocus(|_| Ok(MainCmd::SetParam(*id, f64::NEG_INFINITY).send())).add_loc(loc!())?;
        }.report_err();
        false
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let ButtonProps{style, children, ..} = ctx.props();
        html!{
            <div class="input button" style={*style}
            onpointerdown={ctx.link().callback(Cmd::Focus)}
            onpointerup={ctx.link().callback(Cmd::Unfocus)}
            onpointerenter={ctx.link().callback(Cmd::HoverIn)}
            onpointerleave={ctx.link().callback(Cmd::HoverOut)}>
                {children.clone()}
            </div>
        }
    }
}
