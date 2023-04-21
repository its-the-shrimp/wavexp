#![allow(non_camel_case_types)] // because derive(yew::Properties) generates them

use std::f64::consts::{PI, TAU};

use wasm_bindgen::{JsCast, JsValue};
use web_sys::{Element, HtmlCanvasElement, PointerEvent, HtmlElement};
use yew::{html, 
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
        OptionExt, JsResult, BoolExt, R64},
    MainCmd, loc, sound::Note, r64};

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
    Play(Note),
    Disconnect(usize),
    Remove(usize),
    ToggleWavePitchType(usize, u32),
    WavePitch(usize, u32),
    WaveType(usize, u32),
    RemoveWave(usize, u32),
    AddWave(usize),
    EnvelopeAttack(usize),
    EnvelopeDecay(usize),
    EnvelopSustain(usize),
    EnvelopeRelease(usize),
    BPM(usize),
    DisplayInterval(usize),
    SnapStep(usize)
}

impl ParamId {
    /// returns Some(id) when the parameter ID belongs to a specific element
    pub fn element_id(&self) -> Option<usize> {
        match self {
            ParamId::Play(_)                    => None,
            ParamId::Disconnect(_)              => None,
            ParamId::Remove(_)                  => None,
            ParamId::ToggleWavePitchType(id, _) => Some(*id),
            ParamId::WavePitch(id, _)           => Some(*id),
            ParamId::WaveType(id, _)            => Some(*id),
            ParamId::RemoveWave(id, _)          => Some(*id),
            ParamId::AddWave(id)                => Some(*id),
            ParamId::EnvelopeAttack(id)         => Some(*id),
            ParamId::EnvelopeDecay(id)          => Some(*id),
            ParamId::EnvelopSustain(id)         => Some(*id),
            ParamId::EnvelopeRelease(id)        => Some(*id),
            ParamId::BPM(id)                    => Some(*id),
            ParamId::DisplayInterval(id)        => Some(*id),
            ParamId::SnapStep(id)               => Some(*id)
        }
    }
}

pub struct Slider {
    value: R64,
    canvas: HtmlCanvasElement,
    focused: bool,
    hovered: bool
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
    pub initial: R64
}

const LINE_WIDTH: f64 = 10.0;
const FONT: &'static str = "4em consolas";

impl Component for Slider {
    type Message = Cmd;
    type Properties = SliderProps;

    fn create(ctx: &Context<Self>) -> Self {
        let SliderProps {min, max, initial, ..} = ctx.props();
        Self {focused: false, hovered: false, value: (initial - min) / (max - min),
            canvas: JsValue::UNDEFINED.unchecked_into()}
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        _ = js_try!{type = !:
            let SliderProps{id, signed, min, max, name, ..} = ctx.props();
            msg.handle_hover(name, |_, hovered| Ok(self.hovered = hovered)).add_loc(loc!())?
                .handle_focus(|_| Ok(self.focused = true)).add_loc(loc!())?
                .handle_unfocus(|_| Ok({
                    self.focused = false;
                    MainCmd::SetParam(*id, self.value * (max - min) + min).send();
                })).add_loc(loc!())?
                .handle_drag(|e| Ok({
                    let target = e.target_dyn_into::<Element>().to_js_result(loc!())?;
                    self.value = (self.value + R64::from(e.movement_y()) / R64::from(target.client_height() * -2))
                        .clamp(signed.choose(r64![-1.0], r64![0.0]), r64![1.0]);
                })).add_loc(loc!())?;
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
            if *self.value != 0.0 {
                ctx.arc_with_anticlockwise(w, h + LINE_WIDTH, r - LINE_WIDTH,
                    PI * 1.5, (*self.value * 2.0 + 1.5) * PI, self.value.is_sign_negative()).add_loc(loc!())?;
                ctx.stroke();
            }
            ctx.set_text_baseline("middle");
            ctx.fill_text_with_max_width(&format!("{:.*}", precision, *self.value * (**max - **min) + **min),
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
    pub initial: usize
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
            let SwitchProps {options, id, name, ..} = ctx.props();
            msg.handle_hover(name, |_, hovered| Ok(self.hovered = hovered)).add_loc(loc!())?
                .handle_focus(|_| Ok(self.focused = true)).add_loc(loc!())?
                .handle_unfocus(|_| Ok(self.focused = false)).add_loc(loc!())?
                .handle_drag(|e| {
                    let old_value = *self.value as usize;
                    let target = e.target_dyn_into::<Element>().to_js_result(loc!())?;
                    self.value = R64::rem_euclid(self.value + R64::from(e.movement_y()) / R64::from(target.client_height() / -4 * options.len() as i32),
                        R64::from(options.len())).to_js_result(loc!())?;
                    if old_value != *self.value as usize {
                        MainCmd::SetParam(*id, self.value.floor()).send()}
                    Ok(())})?;
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
    #[prop_or_default]
    pub class: Classes,
    pub desc: AttrValue,
    pub children: Children,
    pub id: ParamId,
    #[prop_or(false)]
    pub svg: bool
}

impl Component for Button {
    type Message = Cmd;
    type Properties = ButtonProps;

    fn create(_: &Context<Self>) -> Self {Self}

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        _ = js_try!{
            let ButtonProps{desc, id, ..} = ctx.props();
            msg.handle_hover(desc, |_, _| Ok(())).add_loc(loc!())?
                .handle_focus(|_| Ok(MainCmd::SetParam(*id, R64::INFINITY).send())).add_loc(loc!())?
                .handle_unfocus(|_| Ok(MainCmd::SetParam(*id, R64::NEG_INFINITY).send())).add_loc(loc!())?;
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
                <div {class}
                onpointerdown={ctx.link().callback(Cmd::Focus)}
                onpointerup={ctx.link().callback(Cmd::Unfocus)}
                onpointerenter={ctx.link().callback(Cmd::HoverIn)}
                onpointerleave={ctx.link().callback(Cmd::HoverOut)}>
                    {ctx.props().children.clone()}
                </div>
            }
        }
    }
}
