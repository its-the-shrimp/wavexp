use std::rc::Rc;
use wasm_bindgen::JsCast;
use yew::TargetCast;
use crate::{utils::{self, ResultUtils, JsResultUtils, Tee}, MainCmd};

pub struct Slider {
    value: f64,
    id: Rc<str>,
    focused: bool
}

#[derive(PartialEq, yew::Properties)]
pub struct SliderProps {
    #[prop_or(yew::classes!("default-input"))]
    pub class: yew::html::Classes,
    pub name: Rc<str>,
    #[prop_or(1.0)]
    pub coef: f64,
    #[prop_or(2)]
    pub precision: usize,
    #[prop_or_default]
    pub postfix: yew::AttrValue,
    pub component_id: usize,
    pub id: usize,
    pub initial: f64
}

pub enum InputCmd {
    ChangeValue(web_sys::PointerEvent),
    Focus(web_sys::PointerEvent),
    Unfocus(web_sys::PointerEvent),
    MaybeShowHelp(),
    MaybeHideHelp()
}

const LINE_WIDTH: f64 = 10.0;
const FONT: &'static str = "4em consolas";

impl yew::Component for Slider {
    type Message = InputCmd;
    type Properties = SliderProps;

    fn create(ctx: &yew::Context<Self>) -> Self {
        let SliderProps {coef, initial, ..} = ctx.props();
        Self {focused: false, value: initial / coef,
            id: uuid::Uuid::new_v4().to_string().into()}
    }

    fn update(&mut self, ctx: &yew::Context<Self>, msg: Self::Message) -> bool {
        match msg {
            InputCmd::ChangeValue(e) => {
                self.value = match e.target_unchecked_into::<web_sys::Element>().client_height() {
                    0 => self.value,
                    h => (self.value + e.movement_y() as f64 / (h * -2) as f64).clamp(0.0, 1.0)};
                let SliderProps {component_id, id, coef, ..} = ctx.props();
                MainCmd::SetParam(*component_id, *id, self.value * coef).send();
                true}
            InputCmd::Focus(e) => {
                let target = e.target()
                    .expect_throw("fetching the input element in the `pointerdown` event")
                    .unchecked_into::<web_sys::HtmlElement>();
                target.set_pointer_capture(e.pointer_id())
                    .expect_throw_val("setting the cursor focus on the input switch");
                self.focused = true;
                MainCmd::SetDesc(ctx.props().name.clone()).send();
                true}
            InputCmd::Unfocus(e) => {
                let target = e.target()
                    .expect_throw("fetching the input element in the `pointerup` event")
                    .unchecked_into::<web_sys::HtmlElement>();
                target.release_pointer_capture(e.pointer_id())
                    .expect_throw_val("releasing the cursor focus from the input switch");
                self.focused = false;
                MainCmd::RemoveDesc().send();
                true}
            InputCmd::MaybeShowHelp() => {
                if !self.focused {
                    MainCmd::SetDesc(ctx.props().name.clone()).send()}
                false}
            InputCmd::MaybeHideHelp() => {
                if !self.focused {
                    MainCmd::RemoveDesc().send()}
                false}
        }
    }

	fn view(&self, ctx: &yew::Context<Self>) -> yew::Html {
        yew::html! {
            <canvas id={self.id.clone()} class={ctx.props().class.clone()}
            onpointerdown={ctx.link().callback(InputCmd::Focus)}
            onpointerup={ctx.link().callback(InputCmd::Unfocus)}
            onpointercancel={ctx.link().callback(InputCmd::Unfocus)}
            onpointerenter={ctx.link().callback(|_| InputCmd::MaybeShowHelp())}
            onpointerleave={ctx.link().callback(|_| InputCmd::MaybeHideHelp())}
            onpointermove={self.focused.then(|| ctx.link().callback(InputCmd::ChangeValue))}/>}
    }

    fn rendered(&mut self, ctx: &yew::Context<Self>, first_render: bool) {
        use std::f64::consts::PI;
        let SliderProps{coef, precision, postfix, ..} = ctx.props();
        if first_render {utils::sync_canvas(&self.id)}
        let (w, h, ctx) = unsafe {utils::get_canvas_ctx(&self.id, &Default::default())
            .unwrap_unchecked()};
        let (w, h) = (w / 2.0, h / 2.0);
// this is safe because by the time this function is called,
// the canvas for the slider itself is already rendered
        if first_render {
            ctx.set_fill_style(&"#0069E1".into());
            ctx.set_stroke_style(&"#0069E1".into());
            ctx.set_line_width(LINE_WIDTH);
            ctx.set_font(FONT);
            ctx.set_text_align("center")}
        ctx.clear_rect(0.0, 0.0, w * 2.0, h * 2.0);
        ctx.begin_path();
        let r = w.min(h) - LINE_WIDTH * 1.5; // 1.5 is an arbitrary multiplier without which the
                                        // slider would go slightly over the edge, idk why
        if self.value != 0.0 {
            ctx.arc(w, h + LINE_WIDTH, r,
                PI * 1.5, (self.value as f64 * 2.0 + 1.5) * PI)
                .expect_throw_val("drawing the slider");
            ctx.stroke()}
        ctx.set_text_baseline("middle");
        ctx.fill_text_with_max_width(&format!("{:.*}", precision, coef * self.value),
            w, h + LINE_WIDTH / 2.0, r)
            .expect_throw_val("drawing the text on the slider");
        ctx.set_text_baseline("top");
        ctx.fill_text_with_max_width(postfix, 
            w, h + LINE_WIDTH * 2.0 + ctx.measure_text("0")
                .expect_throw_val("getting the font height while rendering the slider")
                .font_bounding_box_ascent() * 2.0, r * 1.5)
            .expect_throw_val("drawing the unit name below the slider's value");
    }
}

pub struct Switch {
    value: f64,
    id: Rc<str>,
    focused: bool
}

#[derive(PartialEq, yew::Properties)]
pub struct SwitchProps {
    #[prop_or(yew::classes!("default-input"))]
    pub class: yew::Classes,
    pub name: Rc<str>,
    pub options: Vec<Rc<str>>,
    pub component_id: usize,
    pub id: usize,
    pub initial: usize
}

impl yew::Component for Switch {
    type Message = InputCmd;
    type Properties = SwitchProps;

    fn create(ctx: &yew::Context<Self>) -> Self {
        Self {focused: false, value: ctx.props().initial as f64,
            id: uuid::Uuid::new_v4().to_string().into()}
    }

    fn update(&mut self, ctx: &yew::Context<Self>, msg: Self::Message) -> bool {
        let SwitchProps {options, component_id, id, name, ..} = ctx.props();
        match msg {
            InputCmd::ChangeValue(e) => {
                let old_value = self.value as usize;
                self.value = match e.target_dyn_into::<web_sys::Element>() {
                    None => return false,
                    Some(t) => (self.value + e.movement_y() as f64 / t.client_height() as f64 / -0.5)
                        .rem_euclid(options.len() as f64)
                };
                if old_value == self.value as usize {return false}
                MainCmd::SetParam(*component_id, *id, self.value).send();
                true}
            InputCmd::Focus(e) => {
                e.target()
                    .expect_throw("fetching the input element in the `pointerdown` event")
                    .unchecked_into::<web_sys::HtmlElement>()
                    .set_pointer_capture(e.pointer_id())
                    .expect_throw_val("setting the cursor focus on the input switch");
                self.focused = true;
                MainCmd::SetDesc(name.clone()).send();
                true}
            InputCmd::Unfocus(e) => {
                e.target()
                    .expect_throw("fetching the input element in the `pointerup` event")
                    .unchecked_into::<web_sys::HtmlElement>()
                    .release_pointer_capture(e.pointer_id())
                    .expect_throw_val("releasing the cursor focus from the input switch");
                self.focused = false;
                MainCmd::RemoveDesc().send();
                true}
            InputCmd::MaybeShowHelp() => {
                MainCmd::SetDesc(name.clone()).send();
                false}
            InputCmd::MaybeHideHelp() => {
                MainCmd::RemoveDesc().send();
                false}
        }
    }

	fn view(&self, ctx: &yew::Context<Self>) -> yew::Html {
        yew::html! {
            <canvas id={self.id.clone()} class={ctx.props().class.clone()}
            onpointerdown={ctx.link().callback(InputCmd::Focus)}
            onpointerup={ctx.link().callback(InputCmd::Unfocus)}
            onpointercancel={ctx.link().callback(InputCmd::Unfocus)}
            onpointerenter={ctx.link().callback(|_| InputCmd::MaybeShowHelp())}
            onpointerleave={ctx.link().callback(|_| InputCmd::MaybeHideHelp())}
            onpointermove={self.focused.then(|| ctx.link().callback(InputCmd::ChangeValue))}/>}
    }

    fn rendered(&mut self, ctx: &yew::Context<Self>, first_render: bool) {
        use std::f64::consts::PI;
        let SwitchProps{options, ..} = ctx.props();
        if first_render {utils::sync_canvas(&self.id)}
        let (w, h, ctx) = unsafe {utils::get_canvas_ctx(&self.id, &Default::default())
            .unwrap_unchecked()};
// this is safe because by the time this function is called,
// the canvas for the slider itself is already rendered
        let (w, h) = (w / 2.0, h / 2.0);
        if first_render {
            ctx.set_fill_style(&"#0069E1".into());
            ctx.set_stroke_style(&"#0069E1".into());
            ctx.set_line_width(LINE_WIDTH);
            ctx.set_font(FONT);
            ctx.set_text_align("center");
            ctx.set_text_baseline("middle")}
        ctx.clear_rect(0.0, 0.0, w * 2.0, h * 2.0);
        let r = w.min(h) - LINE_WIDTH * 1.5; // 1.5 is an arbitrary multiplier without which the
                                        // slider would go slightly over the edge, idk why
        let index = self.value.floor();
        ctx.begin_path();
        ctx.arc(w, h + LINE_WIDTH, r, (index / 2.0 + 1.5) * PI, index / 2.0 * PI)
            .expect_throw_val("drawing the switch");
        ctx.stroke();
        ctx.fill_text_with_max_width(unsafe{options.get_unchecked(index as usize)},
            w, h + LINE_WIDTH, r * 1.5)
            .expect_throw_val("drawing the chosen option name on a switch");
    }
}
