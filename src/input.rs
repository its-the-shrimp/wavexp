use std::rc::Rc;
use wasm_bindgen::{UnwrapThrowExt, JsCast};
use yew::TargetCast;
use crate::utils::{self, ExpectThrowVal};

pub struct Slider {
    value: f64,
    onpointermove: js_sys::Function,
    id: Rc<str>
}

#[derive(PartialEq, yew::Properties)]
pub struct SliderProps {
    #[prop_or(1.0)]
    pub coef: f64,
    #[prop_or(2)]
    pub precision: usize,
    #[prop_or(yew::classes!("default-input"))]
    pub class: yew::html::Classes,
    #[prop_or_default]
    pub postfix: yew::AttrValue,
    pub oninput: yew::Callback<f64>,
    pub onfocus: yew::Callback<()>,
    pub onunfocus: yew::Callback<()>
}

pub enum SliderCmd {
    ChangeValue(f64),
    Focus(web_sys::HtmlElement),
    Unfocus(web_sys::HtmlElement)
}

impl yew::Component for Slider {
    type Message = SliderCmd;
    type Properties = SliderProps;

    fn create(ctx: &yew::Context<Self>) -> Self {
        let c: &'static yew::Callback<_> = Box::leak(ctx.link().callback(|e: web_sys::PointerEvent| 
            SliderCmd::ChangeValue(
                e.movement_y() as f64 / e.target_unchecked_into::<web_sys::Element>().client_height() as f64 / -2.0)
        ).into());
        Self {value: 0.0,
            onpointermove: wasm_bindgen::closure::Closure::<dyn Fn(_)>::new(|e: web_sys::PointerEvent| c.emit(e))
                .into_js_value().unchecked_into(),
            id: uuid::Uuid::new_v4().to_string().into()}
    }

    fn update(&mut self, ctx: &yew::Context<Self>, msg: Self::Message) -> bool {
        match msg {
            SliderCmd::ChangeValue(val) => {
                self.value = (self.value + val).clamp(0.0, 1.0);
                ctx.props().oninput.emit(self.value * ctx.props().coef);
                true}
            SliderCmd::Focus(target) => {
                target.set_onpointermove(Some(&self.onpointermove));
                ctx.props().onfocus.emit(());
                false}
            SliderCmd::Unfocus(target) => {
                target.set_onpointermove(None);
                ctx.props().onunfocus.emit(());
            false}}
    }

	fn view(&self, ctx: &yew::Context<Self>) -> yew::Html {
        let on_unfocus = ctx.link().callback(|e: web_sys::PointerEvent| {
            let input = e.target()
                .expect_throw("fetching the input element in the `pointerup` event")
                .unchecked_into::<web_sys::HtmlElement>();
            input.release_pointer_capture(e.pointer_id())
                .expect_throw_val("releasing the cursor focus from the input slider");
            SliderCmd::Unfocus(input)});
        yew::html! {
            <canvas id={self.id.clone()} class={ctx.props().class.clone()} width="300" height="300"
            onpointerdown={ctx.link().callback(|e: web_sys::PointerEvent| {
                let input = e.target()
                    .expect_throw("fetching the input element in the `pointerdown` event")
                    .unchecked_into::<web_sys::HtmlElement>();
                input.set_pointer_capture(e.pointer_id())
                    .expect_throw_val("setting the cursor focus on the input slider");
                SliderCmd::Focus(input)})}
            onpointerup={on_unfocus.clone()}
            onpointercancel={on_unfocus}/>}
    }

    fn rendered(&mut self, ctx: &yew::Context<Self>, first_render: bool) {
        use std::f64::consts::PI;
        let SliderProps{coef, precision, postfix, ..} = ctx.props();
        let (w, h, ctx) = unsafe {utils::get_canvas_ctx(&self.id, &Default::default())
            .unwrap_unchecked()};
        let (w, h) = (w as f64 / 2.0, h as f64 / 2.0);
// this is safe because by the time this function is called,
// the canvas for the slider itself is already rendered
        const WIDTH: f64 = 5.0;
        if first_render {
            ctx.set_fill_style(&"#0069E1".into());
            ctx.set_stroke_style(&"#0069E1".into());
            ctx.set_line_width(WIDTH);
            ctx.set_font("2em consolas");
            ctx.set_text_align("center")}
        ctx.clear_rect(0.0, 0.0, w * 2.0, h * 2.0);
        ctx.begin_path();
        let r = w.min(h) - WIDTH * 1.5; // 1.5 is an arbitrary multiplier without which the
                                        // slider would go slightly over the edge, idk why
        if self.value != 0.0 {
            ctx.arc(w, h + WIDTH, r,
                PI * 1.5, (self.value as f64 * 2.0 + 1.5) * PI)
                .expect_throw_val("drawing the slider");
            ctx.stroke()}
        ctx.set_text_baseline("middle");
        ctx.fill_text_with_max_width(&format!("{:.*}", precision, coef * self.value),
            w, h + WIDTH / 2.0, r)
            .expect_throw_val("drawing the text on the slider");
        ctx.set_text_baseline("top");
        ctx.fill_text_with_max_width(postfix, 
            w, h + WIDTH * 2.0 + ctx.measure_text("0")
                .expect_throw_val("getting the font height while rendering the slider")
                .font_bounding_box_ascent() * 2.0, r * 1.5)
            .expect_throw_val("drawing the unit name below the slider's value");
    }
}

pub struct Switch {
    value: f64,
    onpointermove: js_sys::Function,
    id: Rc<str>
}

#[derive(PartialEq, yew::Properties)]
pub struct SwitchProps {
    #[prop_or(yew::classes!("default-input"))]
    pub class: yew::Classes,
    pub options: Vec<Rc<str>>,
    pub oninput: yew::Callback<u8>,
    pub onfocus: yew::Callback<()>,
    pub onunfocus: yew::Callback<()>
}

pub enum SwitchCmd {
    ChangeValue(f64),
    Focus(web_sys::HtmlElement),
    Unfocus(web_sys::HtmlElement)
}

impl yew::Component for Switch {
    type Message = SwitchCmd;
    type Properties = SwitchProps;

    fn create(ctx: &yew::Context<Self>) -> Self {
        let c: &'static yew::Callback<_> = Box::leak(ctx.link().callback(|e: web_sys::PointerEvent| 
            SwitchCmd::ChangeValue(
                e.movement_y() as f64 / e.target_unchecked_into::<web_sys::Element>().client_height() as f64 / -0.5)
        ).into());
        Self {value: 0.0,
            onpointermove: wasm_bindgen::closure::Closure::<dyn Fn(_)>::new(|e: web_sys::PointerEvent| c.emit(e))
                .into_js_value().unchecked_into(),
            id: uuid::Uuid::new_v4().to_string().into()}
    }

    fn update(&mut self, ctx: &yew::Context<Self>, msg: Self::Message) -> bool {
        match msg {
            SwitchCmd::ChangeValue(val) => {
                let old_value = self.value as u8;
                self.value = (self.value + val).rem_euclid(ctx.props().options.len() as f64);
                if old_value == self.value as u8 {return false}
                ctx.props().oninput.emit(self.value as u8);
                true}
            SwitchCmd::Focus(target) => {
                target.set_onpointermove(Some(&self.onpointermove));
                ctx.props().onfocus.emit(());
                false}
            SwitchCmd::Unfocus(target) => {
                target.set_onpointermove(None);
                ctx.props().onunfocus.emit(());
            false}}
    }

	fn view(&self, ctx: &yew::Context<Self>) -> yew::Html {
        let on_unfocus = ctx.link().callback(|e: web_sys::PointerEvent| {
            let input = e.target()
                .expect_throw("fetching the input element in the `pointerup` event")
                .unchecked_into::<web_sys::HtmlElement>();
            input.release_pointer_capture(e.pointer_id())
                .expect_throw_val("releasing the cursor focus from the input switch");
            SwitchCmd::Unfocus(input)});
        yew::html! {
            <canvas id={self.id.clone()} class={ctx.props().class.clone()}
            onpointerdown={ctx.link().callback(|e: web_sys::PointerEvent| {
                let input = e.target()
                    .expect_throw("fetching the input element in the `pointerdown` event")
                    .unchecked_into::<web_sys::HtmlElement>();
                input.set_pointer_capture(e.pointer_id())
                    .expect_throw_val("setting the cursor focus on the input switch");
                SwitchCmd::Focus(input)})}
            onpointerup={on_unfocus.clone()}
            onpointercancel={on_unfocus}/>}
    }

    fn rendered(&mut self, ctx: &yew::Context<Self>, first_render: bool) {
        use std::f64::consts::PI;
        let SwitchProps{options, ..} = ctx.props();
        let (w, h, ctx) = unsafe {utils::get_canvas_ctx(&self.id, &Default::default())
            .unwrap_unchecked()};
// this is safe because by the time this function is called,
// the canvas for the slider itself is already rendered
        let (w, h) = (w as f64 / 2.0, h as f64 / 2.0);
        const WIDTH: f64 = 5.0;
        if first_render {
            ctx.set_stroke_style(&"#0069E1".into());
            ctx.set_line_width(WIDTH);
            ctx.set_font("2em consolas");
            ctx.set_text_align("center");
            ctx.set_text_baseline("middle")}
        ctx.clear_rect(0.0, 0.0, w * 2.0, h * 2.0);
        let r = w.min(h) - WIDTH * 1.5; // 1.5 is an arbitrary multiplier without which the
                                        // slider would go slightly over the edge, idk why
        let index = self.value.floor();
        ctx.begin_path();
        ctx.arc(w, h + WIDTH, r, (index / 2.0 + 1.5) * PI, index / 2.0 * PI)
            .expect_throw_val("drawing the switch");
        ctx.stroke();
        ctx.fill_text_with_max_width(unsafe{options.get_unchecked(index as usize)},
            w, h + WIDTH, r * 1.5)
            .expect_throw_val("drawing the chosen option name on a switch");
    }
}
