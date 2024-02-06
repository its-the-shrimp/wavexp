use crate::{
    ctx::AppEvent,
    img,
    popup::Popup,
    sound::{AudioInput, Beats},
    visual::{GraphEditor, GraphPoint},
};
use macro_rules_attribute::apply;
use std::{
    marker::PhantomData,
    ops::{Add, Deref, DerefMut, Div, Mul},
};
use wavexp_utils::{
    cell::Shared,
    error::AppError,
    ext::default,
    ext::{BoolExt, HtmlCanvasExt, HtmlElementExt, ResultExt},
    fallible, r64,
    real::R64,
    Pipe, Point,
};
use web_sys::{Element, HtmlCanvasElement, KeyboardEvent, MouseEvent, PointerEvent};
use yew::{
    classes, function_component, html, html::Children, AttrValue, Callback, Classes, Component,
    Context, Html, NodeRef, Properties, TargetCast,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Buttons {
    pub left: bool,
    pub shift: bool,
    pub meta: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Cursor {
    pub point: Point,
    buttons: Buttons,
}

impl Deref for Cursor {
    type Target = Buttons;
    fn deref(&self) -> &Self::Target {
        &self.buttons
    }
}

impl DerefMut for Cursor {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.buttons
    }
}

impl Add<&KeyboardEvent> for Cursor {
    type Output = Self;
    fn add(mut self, rhs: &KeyboardEvent) -> Self::Output {
        self.shift = rhs.shift_key();
        self.meta = rhs.meta_key();
        self
    }
}

impl TryFrom<&MouseEvent> for Cursor {
    type Error = AppError;

    #[apply(fallible!)]
    fn try_from(value: &MouseEvent) -> Self {
        let canvas: HtmlCanvasElement = value.target_dyn_into()?;
        let point = Point {
            x: value.offset_x(),
            y: value.offset_y(),
        }
        .normalise(canvas.client_rect(), canvas.rect())?;
        Self {
            point,
            buttons: Buttons {
                left: value.buttons() & 1 == 1,
                shift: value.shift_key(),
                meta: value.meta_key(),
            },
        }
    }
}

impl TryFrom<&PointerEvent> for Cursor {
    type Error = AppError;

    #[apply(fallible!)]
    fn try_from(value: &PointerEvent) -> Self {
        let canvas: HtmlCanvasElement = value.target_dyn_into()?;
        let point = Point {
            x: value.offset_x(),
            y: value.offset_y(),
        }
        .normalise(canvas.client_rect(), canvas.rect())?;
        Self {
            point,
            buttons: Buttons {
                left: value.buttons() & 1 == 1,
                shift: value.shift_key(),
                meta: value.meta_key(),
            },
        }
    }
}

#[derive(Debug)]
pub enum Cmd {
    Drag(PointerEvent),
    Focus(PointerEvent),
    Unfocus(PointerEvent),
}

pub struct Slider {
    old_value: f64,
    value: R64,
    target: NodeRef,
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
    pub initial: R64,
}

impl Component for Slider {
    type Message = Cmd;
    type Properties = SliderProps;

    fn create(ctx: &Context<Self>) -> Self {
        Self {
            value: ctx.props().initial,
            old_value: f64::NAN,
            target: default(),
        }
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        fallible! {
            let SliderProps { setter, min, max, signed, .. } = ctx.props();
            match &msg {
                Cmd::Drag(e) => {
                    self.value = R64::from(e.movement_y())
                        .div(-self.target.cast::<Element>()?.client_height())
                        .div(e.shift_key().choose(400u16, 2))
                        .mul(max - min)
                        .add(self.value)
                        .clamp(signed.choose(-*max, *min), *max)
                }

                Cmd::Focus(e) => {
                    self.target
                        .cast::<Element>()?
                        .set_pointer_capture(e.pointer_id())?;
                    self.old_value = *self.value;
                }

                Cmd::Unfocus(e) => {
                    self.target
                        .cast::<Element>()?
                        .release_pointer_capture(e.pointer_id())?;
                    if self.old_value != *self.value {
                        setter.emit(self.value)
                    }
                    self.old_value = f64::NAN;
                }
            }
            return true
        }
        .report();
        false
    }

    fn changed(&mut self, ctx: &Context<Self>, old_props: &Self::Properties) -> bool {
        let new_initial = ctx.props().initial;
        if old_props.initial != new_initial {
            self.value = new_initial;
            true
        } else {
            false
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let SliderProps {
            name,
            postfix,
            max,
            min,
            fmt,
            ..
        } = ctx.props();
        let scope = ctx.link();
        let selected: AttrValue = if self.value.abs() == *max {
            "M 50 12 A 38 38 0 0 0 50 88 A 38 38 0 1 0 50 12".into()
        } else {
            let p = ((self.value - min) / (max - min) - 0.5f32) * R64::TAU;
            format!(
                "M 50 12 A 38 38 0 {} 0 {} {}",
                (p > 0) as u8,
                p.sin_or(r64!(0)) * 38u8 + 50,
                p.cos_or(r64!(0)) * 38u8 + 50
            )
            .into()
        };
        html! {
            <svg
                ref={self.target.clone()}
                viewBox="0 0 100 100"
                class="input slider"
                data-main-hint={name}
                onpointerdown={scope.callback(Cmd::Focus)}
                onpointerup={scope.callback(Cmd::Unfocus)}
                onpointermove={(!self.old_value.is_nan()).then(|| scope.callback(Cmd::Drag))}
            >
                <circle class="outer" cx="50" cy="50" r="40" />
                <path d={selected} />
                <circle class="inner" cx="50" cy="50" r="38" />
                <text x="50" y="50">{ fmt.emit(self.value) }</text>
                <text x="50" y="65">{ postfix }</text>
            </svg>
        }
    }
}

pub struct Switch {
    value: R64,
    old_value: usize,
    focused: bool,
    target: NodeRef,
}

#[derive(PartialEq, yew::Properties)]
pub struct SwitchProps {
    pub name: AttrValue,
    pub options: Vec<&'static str>,
    pub setter: Callback<usize>,
    pub initial: usize,
}

impl Component for Switch {
    type Message = Cmd;
    type Properties = SwitchProps;

    fn create(ctx: &Context<Self>) -> Self {
        Self {
            value: ctx.props().initial.into(),
            old_value: 0,
            focused: false,
            target: default(),
        }
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        fallible! {
            let SwitchProps {
                options, setter, ..
            } = ctx.props();
            match msg {
                Cmd::Drag(e) => {
                    self.value = R64::from(e.movement_y())
                        .div(self.target.cast::<Element>()?.client_height())
                        .mul(-4i8)
                        .div(options.len())
                        .add(self.value)
                        .rem_euclid(options.len().into())?
                }

                Cmd::Focus(e) => {
                    self.target
                        .cast::<Element>()?
                        .set_pointer_capture(e.pointer_id())?;
                    self.focused = true;
                    self.old_value = self.value.into();
                }

                Cmd::Unfocus(e) => {
                    self.target
                        .cast::<Element>()?
                        .release_pointer_capture(e.pointer_id())?;
                    let value = self.value.into();
                    if self.old_value != value {
                        setter.emit(value)
                    }
                    self.focused = false;
                }
            }
            return true
        }
        .report();
        false
    }

    fn changed(&mut self, ctx: &Context<Self>, old_props: &Self::Properties) -> bool {
        let new_initial = ctx.props().initial;
        if old_props.initial != new_initial {
            self.value = new_initial.into();
            true
        } else {
            false
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let SwitchProps { name, options, .. } = ctx.props();
        let scope = ctx.link();
        let selected = {
            let v = self.value.floor();
            let n_opts = options.len();
            let src = (v / n_opts - 0.5f32) * R64::TAU;
            let dst = ((v + 1u8) / n_opts - 0.5f32) * R64::TAU;
            format!(
                "M {} {} A 38 38 0 0 0 {} {}",
                src.sin_or(r64!(0)) * 38 + 50,
                src.cos_or(r64!(0)) * 38 + 50,
                dst.sin_or(r64!(0)) * 38 + 50,
                dst.cos_or(r64!(0)) * 38 + 50
            )
        };
        html! {
            <svg
                ref={self.target.clone()}
                viewBox="0 0 100 100"
                class="input switch"
                data-main-hint={name}
                onpointerdown={scope.callback(Cmd::Focus)}
                onpointerup={scope.callback(Cmd::Unfocus)}
                onpointermove={self.focused.then(|| scope.callback(Cmd::Drag))}
            >
                <circle class="outer" cx="50" cy="50" r="40" />
                <path d={selected} />
                <circle class="inner" cx="50" cy="50" r="38" />
                <text
                    x="50"
                    y="50"
                >
                    { unsafe { options.get_unchecked(usize::from(self.value)) } }
                </text>
            </svg>
        }
    }
}

pub struct Button;

#[derive(PartialEq, yew::Properties)]
pub struct ButtonProps {
    pub name: AttrValue,
    pub children: Children,
    pub help: Option<AttrValue>,
    #[prop_or_default]
    pub onclick: Callback<PointerEvent>,
    #[prop_or(false)]
    pub svg: bool,
    #[prop_or(false)]
    pub submit: bool,
    #[prop_or_default]
    pub class: Classes,
}

impl Component for Button {
    type Message = ();
    type Properties = ButtonProps;

    fn create(_: &Context<Self>) -> Self {
        Self
    }

    fn update(&mut self, _: &Context<Self>, _: Self::Message) -> bool {
        false
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let ButtonProps {
            name,
            children,
            svg,
            class,
            onclick,
            help,
            submit,
        } = ctx.props();
        let mut class = class.clone();
        class.push("input button");
        if *svg {
            html! {
                <g
                    {class}
                    data-main-hint={name}
                    data-aux-hint={help}
                    onpointerup={onclick}
                >
                    { children.clone() }
                </g>
            }
        } else {
            html! {
                <button
                    {class}
                    type={submit.choose("submit", "button")}
                    data-main-hint={name}
                    data-aux-hint={help}
                    onpointerup={onclick}
                >
                    { children.clone() }
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
    pub id: Option<&'static str>,
}

impl<T: GraphPoint> Component for GraphEditorCanvas<T> {
    type Message = ();
    type Properties = GraphEditorCanvasProps<T>;

    fn create(_: &Context<Self>) -> Self {
        Self(PhantomData)
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let GraphEditorCanvasProps {
            emitter,
            editor,
            id,
        } = ctx.props();
        match editor.get().report() {
            Some(editor) => {
                // TODO: remove the need to store `GraphEditor`s as shared by removing the need to
                // pass them to this component's props by obsoleting `GraphEditor::id` thus only
                // having to pass the underlying `NodeRef`
                let (canvas_id, id) = (*id, editor.id());
                html! {
                    <canvas
                        ref={editor.canvas().clone()}
                        id={canvas_id}
                        onpointerdown={emitter.reform(move  |e| AppEvent::Focus(id, e))}
                        onpointerup={emitter.reform(move    |e| AppEvent::Hover(id, MouseEvent::from(e)))}
                        onpointermove={emitter.reform(move  |e| AppEvent::Hover(id, MouseEvent::from(e)))}
                        onpointerenter={emitter.reform(move |e| AppEvent::Enter(id, MouseEvent::from(e)))}
                        onpointerout={emitter.reform(move   |_| AppEvent::Leave(id))}
                    />
                }
            }
            None => html! { "Error" },
        }
    }

    fn rendered(&mut self, ctx: &Context<Self>, first_render: bool) {
        if !first_render {
            return;
        }
        if let Some(mut x) = ctx.props().editor.get_mut().report() {
            x.init().report();
        }
    }
}

pub struct Counter {
    value: R64,
    old_value: f64,
    target: NodeRef,
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
    pub initial: R64,
}

impl Component for Counter {
    type Message = Cmd;
    type Properties = CounterProps;

    fn create(ctx: &Context<Self>) -> Self {
        Self {
            value: ctx.props().initial,
            old_value: f64::NAN,
            target: default(),
        }
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        fallible! {
            let CounterProps {
                setter, coef, min, ..
            } = ctx.props();
            match msg {
                Cmd::Drag(e) => {
                    self.value = R64::from(e.movement_y())
                        .div(-self.target.cast::<Element>()?.client_height())
                        .mul(coef)
                        .pipe_if(e.shift_key(), |x| x / 200)
                        .add(self.value)
                        .max(*min)
                }

                Cmd::Focus(e) => {
                    self.target
                        .cast::<Element>()?
                        .set_pointer_capture(e.pointer_id())?;
                    self.old_value = *self.value;
                }

                Cmd::Unfocus(e) => {
                    self.target
                        .cast::<Element>()?
                        .release_pointer_capture(e.pointer_id())?;
                    if self.old_value != *self.value {
                        setter.emit(self.value)
                    }
                    self.old_value = f64::NAN;
                }
            }
            return true
        }
        .report();
        false
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let CounterProps {
            name,
            fmt,
            postfix,
            min,
            ..
        } = ctx.props();
        let scope = ctx.link();
        html! {
            <svg
                ref={self.target.clone()}
                viewBox="0 0 100 100"
                class="input counter"
                data-main-hint={name}
                onpointerdown={scope.callback(Cmd::Focus)}
                onpointerup={scope.callback(Cmd::Unfocus)}
                onpointermove={(!self.old_value.is_nan()).then(|| scope.callback(Cmd::Drag))}
            >
                <polygon class="upper" points="6,16 40,16 50,6 60,16 94,16" />
                <text x="50" y="50">{ fmt.emit(self.value) }</text>
                if !postfix.is_empty() {
                    <text x="50" y="70">{ postfix }</text>
                }
                if self.value != *min {
                    <polygon class="lower" points="6,84 40,84 50,94 60,84 94,84" />
                }
            </svg>
        }
    }
}

#[derive(PartialEq, Properties)]
pub struct TabProps {
    pub name: AttrValue,
    pub desc: AttrValue,
    pub selected: bool,
    #[prop_or_default]
    pub setter: Callback<()>,
}

#[function_component]
pub fn Tab(props: &TabProps) -> Html {
    let TabProps {
        name,
        desc,
        setter,
        selected,
    } = props;
    html! {
        <div
            class={selected.then_some("selected")}
            onpointerup={setter.reform(|_| ())}
            data-main-hint={name}
            data-aux-hint={desc}
        >
            <p >{ name }</p>
        </div>
    }
}

#[derive(PartialEq, Properties)]
pub struct AudioInputButtonProps {
    pub bps: Beats,
    pub name: AttrValue,
    pub input: Option<Shared<AudioInput>>,
    pub help: Option<AttrValue>,
    #[prop_or_default]
    pub onclick: Callback<PointerEvent>,
    #[prop_or(false)]
    pub playing: bool,
    #[prop_or_default]
    pub emitter: Callback<AppEvent>,
    #[prop_or_default]
    pub class: Classes,
}

#[function_component]
pub fn AudioInputButton(props: &AudioInputButtonProps) -> Html {
    let AudioInputButtonProps {
        name,
        bps,
        input,
        help,
        onclick,
        playing,
        emitter,
        class,
    } = props;
    match input.as_ref().map(|x| x.get_aware().report()) {
        Some(Some(input)) => html! {
            <Button
                {name}
                {help}
                class={classes!(class.clone(), "wide")}
                onclick={onclick}
            >
                <div
                    class="inner-button-panel"
                >
                    if *playing {
                        <Button
                            name="Stop playing"
                            help="Click to stop the playback"
                            onclick={emitter.reform(move |e: PointerEvent| {
                            e.stop_propagation();
                            AppEvent::StopPlay
                        })}
                        >
                            <img::Stop />
                        </Button>
                    } else {
                        <Button
                            name="Play audio input"
                            help="Click to hear how the input sounds"
                            onclick={{
                            let s = input.outer();
                            emitter.reform(move |e: PointerEvent| {
                                e.stop_propagation();
                                AppEvent::PreparePlay(Some(s.clone()))
                            })
                        }}
                        >
                            <img::Play />
                        </Button>
                    }
                    <p >{ input.desc(*bps) }</p>
                    <Button
                        name="Edit audio input"
                        help="Click to edit the audio input"
                        onclick={{
                        let s = input.outer();
                        emitter.reform(move |e: PointerEvent| {
                            e.stop_propagation();
                            AppEvent::OpenPopup(Popup::EditInput(s.clone()))
                        })
                    }}
                    >
                        <img::Settings />
                    </Button>
                </div>
            </Button>
        },

        Some(None) => html! {
            <Button
                {name}
                {help}
                class={classes!(class.clone(), "wide")}
            >
                <p style="color:red">{ "Failed to access the audio input" }</p>
            </Button>
        },

        None => html! {
            <Button
                {name}
                {help}
                class={classes!(class.clone(), "wide")}
                onclick={onclick}
            >
                <div
                    class="inner-button-panel"
                >
                    <Button
                        class="unavailable"
                        name="Play audio input (not chosen)"
                        help="Choose the audio input for the sound block to play it here"
                        onclick={|e: PointerEvent| e.stop_propagation()}
                    >
                        <img::Play />
                    </Button>
                    <p >{ "Not chosen" }</p>
                    <Button
                        class="unavailable"
                        name="Edit audio input (not chosen)"
                        help="Choose the audio input for the sound block to edit it \
                          by clicking here"
                        onclick={|e: PointerEvent| e.stop_propagation()}
                    >
                        <img::Settings />
                    </Button>
                </div>
            </Button>
        },
    }
}
