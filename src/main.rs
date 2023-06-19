#![feature(try_blocks)]
#![feature(never_type)]
#![feature(unwrap_infallible)]
#![feature(const_slice_index)]
#![feature(const_float_classify)]
#![feature(const_trait_impl)]
#![feature(const_mut_refs)]
#![feature(array_windows)]
#![feature(drain_filter)]
#![feature(associated_type_defaults)]
#![allow(clippy::unit_arg)]
#![allow(clippy::option_map_unit_fn)]

mod visual;
mod utils;
mod input;
mod sound;
mod sequencer;

use std::rc::Rc;
use utils::R32;
use web_sys::{
    console::warn_1,
    PointerEvent,
    HtmlElement,
    SvgElement,
    Element,
    MouseEvent};
use wasm_bindgen::{
    JsCast,
    JsValue,
    closure::Closure as JsClosure};
use js_sys::Function as JsFunction;
use yew::{
    Component,
    Context, Html, html, TargetCast};
use crate::{
    sound::Sound,
    utils::{
        JsResultUtils,
        window, R64, OptionExt, document, HtmlDocumentExt},
    sound::TabInfo,
    visual::{SoundVisualiser, HintHandler, Graphable},
    sequencer::Sequencer,
    input::{AppEvent, Button, Slider}};

pub fn report_err(err: JsValue) {
    warn_1(&err);
    document().element_dyn_into::<HtmlElement>("error-sign").unwrap_throw(loc!())
        .set_hidden(false);
}

struct Main {
    pub sound_visualiser: SoundVisualiser,
    pub sequencer: Sequencer,
    pub hint_handler: Rc<HintHandler>,
    pub js_callback: JsFunction,
    pub tab_id: usize
}

impl Component for Main {
    type Message = AppEvent;
    type Properties = ();

    fn create(ctx: &Context<Self>) -> Self {
        let cb = ctx.link().callback(AppEvent::Redraw);
        let res = Self{sound_visualiser: SoundVisualiser::new().unwrap_throw(loc!()),
            sequencer: Sequencer::new().unwrap_throw(loc!()),
            hint_handler: Rc::new(HintHandler::default()),
            tab_id: 0,
            js_callback: JsClosure::<dyn Fn(f64)>::new(move |x| cb.emit(R64::new_or(r64![0.0], x)))
                .into_js_value().unchecked_into()};
        window().request_animation_frame(&res.js_callback).unwrap_throw(loc!());
        res
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        _ = js_try!{type = !:
            return match msg {
                AppEvent::Redraw(time) => {
                    self.sequencer.poll(time).add_loc(loc!())?;
                    let play_offset = self.sequencer.play_offset(time);
                    self.sequencer.pattern_mut().redraw(&self.hint_handler, play_offset).add_loc(loc!())?;
                    if let Some((_, mut block)) = self.sequencer.selected_block_mut() {
                        block.inner().redraw_tab(self.tab_id, &self.hint_handler, play_offset).add_loc(loc!())?;
                    }
                    self.sound_visualiser.redraw(self.sequencer.visualiser()).add_loc(loc!())?;
                    window().request_animation_frame(&self.js_callback).add_loc(loc!())?;
                    false
                }

                AppEvent::SetTab(tab_id) => {
                    self.tab_id = tab_id;
                    if let Some(next) = self.sequencer.handle_event(msg).add_loc(loc!())? {
                        ctx.link().send_message(next);
                    }
                    true
                }

                msg => {
                    let res = msg.ui_change();
                    self.sound_visualiser.handle_event(msg.clone()).add_loc(loc!())?;
                    if let Some(next) = self.sequencer.handle_event(msg).add_loc(loc!())? {
                        ctx.link().send_message(next);
                    }
                    res
                }
            }
        }.report_err(loc!());
        false
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let block = self.sequencer.selected_block();

        let setter = ctx.link().callback(|x| x);
        let render_tab_info = |info: &TabInfo, tab_id: usize, desc: String| -> Html {
            html!{
                <div id={(self.tab_id == tab_id).then_some("selected-tab")}
                onpointerup={ctx.link().callback(move |_| AppEvent::SetTab(tab_id))}
                data-main-hint={info.name} data-aux-hint={desc}>
                    <p>{info.name}</p>
                </div>
            }
        };

        html! {<>
            <div id="main-panel">
                <div id="ctrl-panel" class="dark-bg"
                data-main-hint="Settings" data-aux-hint={block.map_or_else(|| "General".to_owned(), |(_, x)| x.desc())}>
                    <div id="hint" class="light-bg"
                    data-main-hint="Hint bar" data-aux-hint="for useful messages about the app's controls">
                        <span id="main-hint" ref={self.hint_handler.main_bar().clone()}/>
                        <br/>
                        <span id="aux-hint" ref={self.hint_handler.aux_bar().clone()}/>
                    </div>
                    if let Some((tab_aux_hint, id, block)) = block.map(|(id, block)| (block.desc() + ": Settings tab", id, block)) {
                        <div id="tab-list">
                            {for block.sound.tabs().iter().enumerate()
                                .map(|(tab_id, tab)| render_tab_info(tab, tab_id, tab_aux_hint.clone()))}
                        </div>
                        {block.sound.params(self.tab_id, id, setter.clone())}
                        <div id="general-ctrl" class="dark-bg">
                            <Button name="Back to project-wide settings"
                            setter={setter.reform(|_| AppEvent::Select(None))}>
                                <svg viewBox="0 0 100 100">
                                    <polygon points="20,60 50,20 80,60 70,60 70,80 30,80 30,60"/>
                                </svg>
                            </Button>
                            <Button name="Remove component"
                            setter={setter.reform(move |_| AppEvent::Remove(id))}>
                                <svg viewBox="0 0 100 100">
                                    <polygon points="27,35 35,27 50,42 65,27 73,35 58,50 73,65 65,73 50,58 35,73 27,65 42,50"/>
                                </svg>
                            </Button>
                        </div>
                    } else {
                        // TODO: fix tilted edges of the borders of the tab menu
                        <div id="tab-list">
                            {render_tab_info(&TabInfo{name: "General"}, 0, "Settings tab".to_owned())}
                            {render_tab_info(&TabInfo{name: "Add block"}, 1, "Settings tab".to_owned())}
                        </div>
                        if self.tab_id == 0 {
                            <div id="inputs">
                                <Slider key="tmp" name="Tempo"
                                    setter={setter.reform(AppEvent::Bpm)}
                                    min={r64![30.0]} max={r64![240.0]}
                                    postfix="BPM"
                                    initial={self.sequencer.bps() * r64![60.0]}/>
                                <Slider key="gain" name="Master gain level"
                                setter={setter.reform(|x| AppEvent::MasterGain(R32::from(x)))}
                                    initial={R64::from(self.sequencer.gain())}/>
                            </div>
                        } else if self.tab_id == 1 {
                            <div id="block-add-menu">
                                {for Sound::TYPES.iter().map(|x| html!{
                                    <div draggable="true"
                                    data-main-hint={x.name()} data-aux-hint="Hold and drag to add block to plane"
                                    ondragstart={ctx.link().callback(|_| AppEvent::SetBlockAdd(Some(*x)))}
                                    ondragend={ctx.link().callback(|_| AppEvent::SetBlockAdd(None))}>
                                        <p>{x.name()}</p>
                                    </div>
                                })}
                            </div>
                        }
                    }
                </div>
                <canvas ref={self.sequencer.canvas().clone()} id="plane"
                onpointerdown={ctx.link().callback(    AppEvent::FocusPlane)}
                onpointerup={ctx.link().callback(|e|   AppEvent::HoverPlane(MouseEvent::from(e)))}
                onpointermove={ctx.link().callback(|e| AppEvent::HoverPlane(MouseEvent::from(e)))}
                onpointerout={ctx.link().callback(|_|  AppEvent::LeavePlane)}
                ondragover={ctx.link().callback(|e|    AppEvent::HoverPlane(MouseEvent::from(e)))}
                ondragleave={ctx.link().callback(|_|   AppEvent::LeavePlane)}
                ondrop={ctx.link().callback(|e|        AppEvent::HoverPlane(MouseEvent::from(e)))}/>
            </div>
            <div id="io-panel" data-main-hint="Editor plane settings">
                {self.sequencer.editor_plane_params(setter.clone())}
                <Button name="Play"
                setter={setter.reform(|_| AppEvent::TogglePlay)}>
                    <svg viewBox="0 0 100 100" height="100%">
                        <polygon points="25,25 75,50 25,75"/>
                    </svg>
                </Button>
                <canvas id="sound-visualiser" ref={self.sound_visualiser.canvas().clone()} class="blue-border"
                data-main-hint="Sound visualiser"/>
            </div>
            <div id="error-sign" hidden={true}
            data-main-hint="Error has occured" data-aux-hint="Check the console for more info">
                <svg viewBox="0 0 100 100">
                    <polygon points="10,90 50,10 90,90"/>
                    <polygon points="48,40 52,40 52,60 48,60"/>
                    <polygon points="48,70 52,70 52,74 48,74"/>
                </svg>
            </div>
        </>}
    }

    fn rendered(&mut self, ctx: &Context<Self>, first_render: bool) {
        if !first_render {return}

        let window = window();
        let cb = ctx.link().callback(|_| AppEvent::Resize);
        let cb = JsClosure::<dyn Fn()>::new(move || cb.emit(()))
            .into_js_value().unchecked_into();
        window.set_onresize(Some(&cb));

        fn get_hint(mut element: Element) -> [String; 2] {
            loop {
                let dataset = if let Some(x) = element.dyn_ref::<HtmlElement>() {
                    x.dataset()
                } else if let Some(x) = element.dyn_ref::<SvgElement>() {
                    x.dataset()
                } else {
                    return Default::default()
                };
                if let Some(main) = dataset.get("mainHint") {
                    return [main, dataset.get("auxHint").unwrap_or_default()]
                }
                if let Some(parent) = element.parent_element() {
                    element = parent
                } else {
                    return Default::default()
                }
            }
        }

        let hint_handler = Rc::clone(&self.hint_handler);
        let cb = JsClosure::<dyn Fn(_)>::new(move |e: PointerEvent| {
            let Some(target) = e.target_dyn_into::<Element>()
                .report_err(loc!()) else {return};
            let [main, aux] = get_hint(target);
            _ = hint_handler.set_hint(&main, &aux).report_err(loc!());
        }).into_js_value().unchecked_into();
        window.set_onpointerover(Some(&cb));

        ctx.link().send_message(AppEvent::Resize);
    }
}

fn main() {
    yew::Renderer::<Main>::new().render();
}
