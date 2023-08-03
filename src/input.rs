use std::{ops::{Div, Mul, Add, Deref, DerefMut}, marker::PhantomData};
use wasm_bindgen::JsValue;
use web_sys::{
    Element,
    HtmlCanvasElement,
    PointerEvent,
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
    Callback,
    Properties,
    scheduler::Shared};
use crate::{
    utils::{js_try, JsResultUtils, HtmlCanvasExt, OptionExt, R64, Point, HtmlElementExt, ResultToJsResult, report_err, Pipe, BoolExt, default},
    visual::{GraphPoint, GraphEditor},
    global::AppEvent,
    loc,
    r64};

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
    Unfocus(PointerEvent)
}

pub struct Slider {
    old_value: f64,
    value: R64
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
    #[prop_or(Callback::from(|x| format!("{x:.2}")))]
    pub fmt: Callback<R64, String>,
    #[prop_or("")]
    pub postfix: &'static str,
    pub setter: Callback<R64>,
    pub initial: R64
}

impl Component for Slider {
    type Message = Cmd;
    type Properties = SliderProps;

    fn create(ctx: &Context<Self>) -> Self {
        Self{value: ctx.props().initial, old_value: f64::NAN}
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        js_try!{type = !:
            let SliderProps{setter, min, max, signed, ..} = ctx.props();
            match msg {
                Cmd::Drag(e) => self.value = R64::from(e.movement_y())
                    .div(-e.target_dyn_into::<Element>().to_js_result(loc!())?.client_height())
                    .div(e.shift_key().choose(400u16, 2))
                    .mul(max - min)
                    .add(self.value)
                    .clamp(signed.choose(-*max, *min), *max),

                Cmd::Focus(e) => {
                    e.target_dyn_into::<Element>().to_js_result(loc!())?
                        .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    self.old_value = *self.value;
                }

                Cmd::Unfocus(e) => {
                    e.target_dyn_into::<Element>().to_js_result(loc!())?
                        .release_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    if self.old_value != *self.value {setter.emit(self.value)}
                    self.old_value = f64::NAN;
                }
            }
            return true
        }.report_err(loc!());
        false
    }

    fn changed(&mut self, ctx: &Context<Self>, old_props: &Self::Properties) -> bool {
        let new_initial = ctx.props().initial;
        if old_props.initial != new_initial {
            self.value = new_initial;
            true
        } else {false}
    }

	fn view(&self, ctx: &Context<Self>) -> Html {
        let SliderProps{name, postfix, max, min, fmt, ..} = ctx.props();
        let scope = ctx.link();
        html!{
            <svg viewBox="0 0 100 100" class="input slider" data-main-hint={name}
            onpointerdown={scope.callback(Cmd::Focus)}
            onpointerup={scope.callback(Cmd::Unfocus)}
            onpointermove={(!self.old_value.is_nan()).then(|| scope.callback(Cmd::Drag))}>
                <circle class="outer" cx="50" cy="50" r="40"/>
                <path d={if self.value.abs() == *max {
                    AttrValue::from("M 50 12 A 38 38 0 0 0 50 88 A 38 38 0 1 0 50 12")
                } else {
                    let p = ((self.value - min) / (max - min) - 0.5f32) * R64::TAU;
                    format!("M 50 12 A 38 38 0 {} 0 {} {}",
                        (p > 0) as u8, p.sin_or(r64![0.0]) * 38 + 50, p.cos_or(r64![0.0]) * 38 + 50).into()
                }}/>
                <circle class="inner" cx="50" cy="50" r="38"/>
                <text x="50" y="50">{fmt.emit(self.value)}</text>
                <text x="50" y="65">{postfix}</text>
            </svg>
        }
    }
}

pub struct Switch {
    value: R64,
    old_value: usize,
    focused: bool,
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

    #[inline] fn create(ctx: &Context<Self>) -> Self {
        Self{value: ctx.props().initial.into(), old_value: 0, focused: false}
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        js_try!{type = !:
            let SwitchProps{options, setter, ..} = ctx.props();
            match msg {
                Cmd::Drag(e) => self.value = R64::from(e.movement_y())
                    .div(e.target_dyn_into::<Element>().to_js_result(loc!())?.client_height())
                    .mul(-4i8)
                    .div(options.len())
                    .add(self.value)
                    .rem_euclid(options.len().into()).to_js_result(loc!())?,

                Cmd::Focus(e) => {
                    e.target_dyn_into::<Element>().to_js_result(loc!())?
                        .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    self.focused = true;
                    self.old_value = self.value.into();
                }

                Cmd::Unfocus(e) => {
                    e.target_dyn_into::<Element>().to_js_result(loc!())?
                        .release_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    let value = self.value.into();
                    if self.old_value != value {
                        setter.emit(value)
                    }
                    self.focused = false;
                }
            }
            return true
        }.report_err(loc!());
        false
    }

    #[inline] fn changed(&mut self, ctx: &Context<Self>, old_props: &Self::Properties) -> bool {
        let new_initial = ctx.props().initial;
        if old_props.initial != new_initial {
            self.value = new_initial.into();
            true
        } else {false}
    }

	fn view(&self, ctx: &Context<Self>) -> Html {
        let SwitchProps{name, options, ..} = ctx.props();
        let scope = ctx.link();
        html!{
            <svg viewBox="0 0 100 100" class="input switch" data-main-hint={name}
            onpointerdown={scope.callback(Cmd::Focus)}
            onpointerup={scope.callback(Cmd::Unfocus)}
            onpointermove={self.focused.then(|| scope.callback(Cmd::Drag))}>
                <circle class="outer" cx="50" cy="50" r="40"/>
                <path d={{
                    let v = self.value.floor();
                    let n_opts = options.len();
                    let src = (v / n_opts - 0.5f32) * R64::TAU;
                    let dst = ((v + 1u8) / n_opts - 0.5f32) * R64::TAU;
                    format!("M {} {} A 38 38 0 0 0 {} {}",
                        src.sin_or(r64![0.0]) * 38 + 50, src.cos_or(r64![0.0]) * 38 + 50,
                        dst.sin_or(r64![0.0]) * 38 + 50, dst.cos_or(r64![0.0]) * 38 + 50)
                }}/>
                <circle class="inner" cx="50" cy="50" r="38"/>
                <text x="50" y="50">{unsafe{options.get_unchecked(usize::from(self.value))}}</text>
            </svg>
        }
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

    #[inline] fn update(&mut self, _: &Context<Self>, _: Self::Message) -> bool {false}

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

pub struct GraphEditorCanvas<T>(PhantomData<T>);

#[derive(Debug, PartialEq, Properties)]
pub struct GraphEditorCanvasProps<T: GraphPoint> {
    pub emitter: Callback<AppEvent>,
    pub editor: Shared<GraphEditor<T>>,
    pub id: Option<&'static str>
}

impl<T: 'static + GraphPoint> Component for GraphEditorCanvas<T> {
    type Message = ();
    type Properties = GraphEditorCanvasProps<T>;

    #[inline] fn create(_: &Context<Self>) -> Self {Self(default())}

    fn view(&self, ctx: &Context<Self>) -> Html {
        let GraphEditorCanvasProps{emitter, editor, id} = ctx.props();
        match editor.try_borrow().to_js_result(loc!()) {
            Ok(editor) => {
                let (canvas_id, id) = (*id, editor.id());
                html!{<canvas ref={editor.canvas().clone()} id={canvas_id}
                    onpointerdown={emitter.reform(move  |e| AppEvent::Focus(id, e))}
                    onpointerup={emitter.reform(move    |e| AppEvent::Hover(id, MouseEvent::from(e)))}
                    onpointermove={emitter.reform(move  |e| AppEvent::Hover(id, MouseEvent::from(e)))}
                    onpointerenter={emitter.reform(move |e| AppEvent::Enter(id, MouseEvent::from(e)))}
                    onpointerout={emitter.reform(move   |_| AppEvent::Leave(id))}/>}
            }
            Err(err) => {
                report_err(err);
                html!{"Error"}
            }
        }
    }

    #[inline] fn rendered(&mut self, ctx: &Context<Self>, first_render: bool) {
        if !first_render {return}
        match ctx.props().editor.try_borrow_mut().to_js_result(loc!()) {
            Ok(mut x) => _ = x.init().report_err(loc!()),
            Err(e) => report_err(e)
        }
    }
}

pub struct Counter {
    value: R64,
    old_value: f64
}

#[derive(PartialEq, yew::Properties)]
pub struct CounterProps {
    pub name: AttrValue,
    #[prop_or(R64::ONE)]
    pub coef: R64,
    #[prop_or(R64::ZERO)]
    pub min: R64,
    #[prop_or(Callback::from(|x| format!("{x:.2}")))]
    pub fmt: Callback<R64, String>,
    #[prop_or("")]
    pub postfix: &'static str,
    pub setter: Callback<R64>,
    pub initial: R64
}

impl Component for Counter {
    type Message = Cmd;
    type Properties = CounterProps;

    #[inline] fn create(ctx: &Context<Self>) -> Self {
        Self{value: ctx.props().initial, old_value: f64::NAN}
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        js_try!{type = !:
            let CounterProps{setter, coef, min, ..} = ctx.props();
            match msg {
                Cmd::Drag(e) => self.value = R64::from(e.movement_y())
                    .div(-e.target_dyn_into::<Element>().to_js_result(loc!())?.client_height())
                    .mul(coef)
                    .pipe_if(e.shift_key(), |x| x / 2)
                    .add(self.value)
                    .max(*min),

                Cmd::Focus(e) => {
                    e.target_dyn_into::<Element>().to_js_result(loc!())?
                        .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    self.old_value = *self.value;
                }

                Cmd::Unfocus(e) => {
                    e.target_dyn_into::<Element>().to_js_result(loc!())?
                        .release_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    if self.old_value != *self.value {setter.emit(self.value)}
                    self.old_value = f64::NAN;
                }
            }
            return true
        }.report_err(loc!());
        false
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let CounterProps{name, fmt, postfix, min, ..} = ctx.props();
        let scope = ctx.link();
        html!{
            <svg viewBox="0 0 100 100" class="input counter" data-main-hint={name}
            onpointerdown={scope.callback(Cmd::Focus)}
            onpointerup={scope.callback(Cmd::Unfocus)}
            onpointermove={(!self.old_value.is_nan()).then(|| scope.callback(Cmd::Drag))}>
                <polygon class="upper" points="6,16 40,16 50,6 60,16 94,16"/>
                <text x="50" y="50">{fmt.emit(self.value)}</text>
                if !postfix.is_empty() {
                    <text x="50" y="50">{postfix}</text>
                }
                if self.value != *min {
                    <polygon class="lower" points="6,84 40,84 50,94 60,84 94,84"/>
                }
            </svg>
        }
    }
}
