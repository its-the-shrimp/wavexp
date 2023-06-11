#![feature(try_blocks)]
#![feature(never_type)]
#![feature(unwrap_infallible)]
#![feature(const_slice_index)]
#![feature(const_float_classify)]
#![feature(const_trait_impl)]
#![feature(const_mut_refs)]
#![allow(clippy::unit_arg)]
#![allow(clippy::option_map_unit_fn)]

mod visual;
mod utils;
mod input;
mod sound;
mod sequencer;

use std::{
    fmt::Debug,
    rc::Rc, cell::RefMut};
use input::{ParamId, Button, Slider};
use sound::{SoundType, TabInfo};
use visual::{SoundVisualiser, EditorPlaneHandler, CanvasEvent, HintHandler};
use sequencer::Sequencer;
use utils::{
    JsResultUtils, JsResult,
    MaybeCell, WasmCell,
    window, R64, Point, OptionExt, document, HtmlDocumentExt};
use web_sys::{
    console::warn_1,
    HtmlCanvasElement, PointerEvent, DragEvent, HtmlElement, SvgElement, Element};
use wasm_bindgen::{
    JsCast,
    JsValue,
    closure::Closure as JsClosure};
use js_sys::Function as JsFunction;
use yew::{
    Callback,
    Component,
    Context, Html, html, TargetCast};
use crate::{sound::{Sound, Secs}, utils::ResultToJsResult};

/// responsible for playing sounds and frame-by-frame animations
struct Player {
    pub sound_visualiser: SoundVisualiser,
    pub editor_plane_handler: EditorPlaneHandler,
    pub sequencer: Sequencer,
    pub hint_handler: Rc<HintHandler>,
    pub js_callback: JsFunction
}

static GLOBAL_PLAYER: WasmCell<MaybeCell<Player>> = WasmCell::new(MaybeCell::new());

impl Player {
    fn init_global() -> JsResult<RefMut<'static, Self>> {
        fn render(time: f64) {
            _ = js_try!{type = !:
                let time = Secs::try_from(time).to_js_result(loc!())? / 1000u16;
                let mut handle = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                let Player{ref mut editor_plane_handler, ref mut sound_visualiser,
                    ref mut sequencer, ref mut hint_handler, ref js_callback} = *handle;

                sequencer.poll(time).add_loc(loc!())?;
                if let Some(visualiser) = sequencer.visualiser() {
                    sound_visualiser.poll(visualiser).add_loc(loc!())?;
                }
                editor_plane_handler.poll(sequencer.pattern(), hint_handler,
                    sequencer.cur_play_offset(time)).add_loc(loc!())?;
                window().request_animation_frame(js_callback).add_loc(loc!())?;
                return
            }.report_err(loc!());
        }

        let res = GLOBAL_PLAYER.set(Self{
            sound_visualiser: SoundVisualiser::new().add_loc(loc!())?,
            editor_plane_handler: EditorPlaneHandler::new().add_loc(loc!())?,
            sequencer: Sequencer::new().add_loc(loc!())?,
            hint_handler: Rc::new(HintHandler::default()),
            js_callback: JsClosure::<dyn Fn(f64)>::new(render).into_js_value().unchecked_into()
        }).add_loc(loc!())?;

        window().request_animation_frame(&res.js_callback).add_loc(loc!()).map(|_| res)
    }

    pub fn set_param(&mut self, id: ParamId, value: R64) -> JsResult<bool> {
        Ok(self.sequencer.set_param(id, value).add_loc(loc!())?
            | self.editor_plane_handler.set_param(id, value))
    }

    pub fn handle_resize(&mut self) -> JsResult<()> {
        self.editor_plane_handler.handle_resize().add_loc(loc!())?;
        self.sound_visualiser.handle_resize().add_loc(loc!())?;
        Ok(())
    }
}

pub struct Main {
    editor_tab_id: usize,
    new_block_type: Option<SoundType>
}

#[derive(Debug)]
pub enum MainCmd {
    Hover(PointerEvent),
    Focus(PointerEvent),
    Unfocus(PointerEvent),
    Leave,
    SetParam(ParamId, R64),
    ReportError(JsValue),
    SetTab(usize),
    Resize,
    BlockAddStart(SoundType),
    BlockAddEnd(Option<DragEvent>),
    HoverNewBlock(DragEvent),
    DragNewBlockOut,
}

impl From<ParamId> for MainCmd {
    #[inline]
    fn from(value: ParamId) -> Self {Self::SetParam(value, R64::INFINITY)}
}

static mut MAINCMD_SENDER: Option<Callback<MainCmd>> = None;
impl MainCmd {
    #[inline] pub fn send(self) {
        unsafe{MAINCMD_SENDER.as_ref().unwrap_unchecked()}.emit(self)
    }
}

impl Component for Main {
    type Message = MainCmd;
    type Properties = ();

    fn create(ctx: &Context<Self>) -> Self {
        *unsafe{&mut MAINCMD_SENDER} = Some(ctx.link().callback(|msg| msg));
        Player::init_global().unwrap_throw(loc!());
        Self{editor_tab_id: 0, new_block_type: None}
    }

    fn update(&mut self, _: &Context<Self>, msg: Self::Message) -> bool {
        let on_new_error = |err: JsValue| -> bool {
            warn_1(&err);
            document().element_dyn_into::<HtmlElement>("error-sign").unwrap_throw(loc!())
                .set_hidden(false);
            false
        };

        let err = js_try!{type = !:
            return match msg {
                MainCmd::Hover(e) => {
                    let e = CanvasEvent::try_from(&*e).add_loc(loc!())?;
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    let Player{ref mut editor_plane_handler, ref mut sequencer, ..} = *player;
                    if let Some((id, value)) = editor_plane_handler.handle_hover(Some(e), sequencer.pattern_mut()) {
                        player.set_param(id, value).add_loc(loc!())? | self.set_param(id, value)
                    } else {false}
                }

                MainCmd::Focus(e) => {
                    e.target_unchecked_into::<HtmlCanvasElement>()
                        .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    let e = CanvasEvent::try_from(&*e).add_loc(loc!())?;
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    let Player{ref mut editor_plane_handler, ref mut sequencer, ..} = *player;
                    if let Some((id, value)) = editor_plane_handler.handle_hover(Some(e), sequencer.pattern_mut()) {
                        player.set_param(id, value).add_loc(loc!())? | self.set_param(id, value)
                    } else {false}
                }

                MainCmd::Unfocus(e) => {
                    e.target_unchecked_into::<HtmlCanvasElement>()
                        .release_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    let e = CanvasEvent::try_from(&*e).add_loc(loc!())?;
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    let Player{ref mut editor_plane_handler, ref mut sequencer, ..} = *player;
                    if let Some((id, value)) = editor_plane_handler.handle_hover(Some(e), sequencer.pattern_mut()) {
                        player.set_param(id, value).add_loc(loc!())? | self.set_param(id, value)
                    } else {false}
                }

                MainCmd::Leave => {
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    let Player{ref mut editor_plane_handler, ref mut sequencer, ..} = *player;
                    if let Some((id, value)) = editor_plane_handler.handle_hover(None, sequencer.pattern_mut()) {
                        player.set_param(id, value).add_loc(loc!())? | self.set_param(id, value)
                    } else {false}
                }

                MainCmd::SetParam(id, value) => {
                    self.set_param(id, value);
                    GLOBAL_PLAYER.get_mut().add_loc(loc!())?
                        .set_param(id, value).add_loc(loc!())?
                }

                MainCmd::SetTab(id) => {
                    self.editor_tab_id = id;
                    true
                }

                MainCmd::Resize => {
                    GLOBAL_PLAYER.get_mut().add_loc(loc!())?
                        .handle_resize().add_loc(loc!())?;
                    true
                }

                MainCmd::BlockAddStart(decl) => {
                    self.new_block_type = Some(decl);
                    GLOBAL_PLAYER.get_mut().add_loc(loc!())?
                        .editor_plane_handler.handle_block_add(Some(decl), None);
                    false
                }

                MainCmd::BlockAddEnd(e) => {
                    self.new_block_type = None;
                    let at = e.map(|e| Point{x: e.offset_x(), y: e.offset_y()});
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    if let Some((id, val)) = player.editor_plane_handler.handle_block_add(None, at) {
                        player.set_param(id, val).add_loc(loc!())? | self.set_param(id, val)
                    } else {false}
                }

                MainCmd::HoverNewBlock(e) => {
                    // TODO: somehow hide the moved copy of the element being dragged
                    e.prevent_default();
                    let at = Some(Point{x: e.offset_x(), y: e.offset_y()});
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    if let Some((id, val)) = player.editor_plane_handler.handle_block_add(self.new_block_type, at) {
                        player.set_param(id, val).add_loc(loc!())? | self.set_param(id, val)
                    } else {false}
                }

                MainCmd::DragNewBlockOut => {
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    if let Some((id, val)) = player.editor_plane_handler.handle_block_add(self.new_block_type, None) {
                        player.set_param(id, val).add_loc(loc!())? | self.set_param(id, val)
                    } else {false}
                }

                MainCmd::ReportError(err) => return on_new_error(err)
            }
        };
        on_new_error(err.into_err())
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let player = GLOBAL_PLAYER.get().unwrap_throw(loc!());
        let block = player.editor_plane_handler.selected_element_id()
            .map(|x| (x, unsafe{player.sequencer.pattern().get_unchecked(x)}));

        let render_tab_info = |info: &TabInfo, block_id: usize, tab_id: usize, desc: String| -> Html {
            if self.editor_tab_id == tab_id {
                if info.dynamic {
                    ctx.link().send_message(MainCmd::SetParam(ParamId::Redraw(block_id), tab_id.into()));
                }
                html!{
                    <div id="selected-tab"
                    onpointerup={ctx.link().callback(move |_| MainCmd::SetTab(tab_id))}
                    data-main-hint={info.name} data-aux-hint={desc}>
                        <p onpointerup={ctx.link().callback(|_| MainCmd::SetTab(0))}>{info.name}</p>
                    </div>
                }
            } else {
                html!{
                    <div onpointerup={ctx.link().callback(move |_| MainCmd::SetTab(tab_id))}
                    data-main-hint={info.name} data-aux-hint={desc}>
                        <p onpointerup={ctx.link().callback(|_| MainCmd::SetTab(0))}>{info.name}</p>
                    </div>
                }
            }
        };

        html! {<>
            <div id="main-panel">
                <div id="ctrl-panel" class="dark-bg"
                data-main-hint="Settings" data-aux-hint={block.map_or_else(|| "General".to_owned(), |(_, x)| x.desc())}>
                    <div id="hint" class="light-bg"
                    data-main-hint="Hint bar" data-aux-hint="for useful messages about the app's controls">
                        <span id="main-hint" ref={player.hint_handler.main_bar().clone()}/>
                        <br/>
                        <span id="aux-hint" ref={player.hint_handler.aux_bar().clone()}/>
                    </div>
                    if let Some((tab_aux_hint, id, block)) = block.map(|(id, block)| (block.desc() + ": Settings tab", id, block)) {
                        <div id="tab-list">
                            {for block.sound.tabs().iter().enumerate()
                                .map(|(tab_id, tab)| render_tab_info(tab, id, tab_id, tab_aux_hint.clone()))}
                        </div>
                        {block.sound.params(self.editor_tab_id, id)}
                        <div id="general-ctrl" class="dark-bg">
                            <Button id={ParamId::Select} name="Back to project-wide settings">
                                <svg viewBox="0 0 100 100">
                                    <polygon points="20,60 50,20 80,60 70,60 70,80 30,80 30,60"/>
                                </svg>
                            </Button>
                            <Button id={ParamId::Remove(id)} name="Remove component">
                                <svg viewBox="0 0 100 100">
                                    <polygon points="27,35 35,27 50,42 65,27 73,35 58,50 73,65 65,73 50,58 35,73 27,65 42,50"/>
                                </svg>
                            </Button>
                        </div>
                    } else {
                        // TODO: fix tilted edges of the borders of the tab menu
                        <div id="tab-list">
                            <div id={(self.editor_tab_id == 0).then_some("selected-tab")}
                            onpointerup={ctx.link().callback(|_| MainCmd::SetTab(0))}
                            data-main-hint="General" data-aux-hint="Settings tab">
                                <p onpointerup={ctx.link().callback(|_| MainCmd::SetTab(0))}>{"General"}</p>
                            </div>
                            <div id={(self.editor_tab_id == 1).then_some("selected-tab")}
                            onpointerup={ctx.link().callback(|_| MainCmd::SetTab(1))}
                            data-main-hint="Add Block" data-aux-hint="Settings tab">
                                <p onpointerup={ctx.link().callback(|_| MainCmd::SetTab(1))}>{"Add Block"}</p>
                            </div>
                        </div>
                        if self.editor_tab_id == 0 {
                            <div id="inputs">
                                <Slider key="tmp" name="Tempo"
                                    id={ParamId::Bpm}
                                    min={r64![30.0]} max={r64![240.0]}
                                    postfix="BPM"
                                    initial={player.sequencer.bps() * r64![60.0]}/>
                                <Slider key="gain" name="Master gain level"
                                    id={ParamId::MasterGain}
                                    initial={R64::from(player.sequencer.gain())}/>
                            </div>
                        } else if self.editor_tab_id == 1 {
                            <div id="block-add-menu">
                                {for Sound::TYPES.iter().map(|x| html!{
                                    <div draggable="true"
                                    data-main-hint={x.name()} data-aux-hint="Hold and drag to add block to plane"
                                    ondragstart={ctx.link().callback(|_| MainCmd::BlockAddStart(*x))}
                                    ondragend={ctx.link().callback(|_| MainCmd::BlockAddEnd(None))}>
                                        <p>{x.name()}</p>
                                    </div>
                                })}
                            </div>
                        }
                    }
                </div>
                <canvas ref={player.editor_plane_handler.canvas().clone()} id="plane"
                onpointerdown={ctx.link().callback(MainCmd::Focus)}
                onpointerup={ctx.link().callback(MainCmd::Unfocus)}
                onpointermove={ctx.link().callback(MainCmd::Hover)}
                onpointerout={ctx.link().callback(|_| MainCmd::Leave)}
                ondragover={ctx.link().callback(MainCmd::HoverNewBlock)}
                ondragleave={ctx.link().callback(|_| MainCmd::DragNewBlockOut)}
                ondrop={ctx.link().callback(|e| MainCmd::BlockAddEnd(Some(e)))}/>
            </div>
            <div id="io-panel" data-main-hint="Editor plane settings">
                {player.editor_plane_handler.params()}
                <Button id={ParamId::Play} name="Play">
                    <svg viewBox="0 0 100 100" height="100%">
                        <polygon points="25,25 75,50 25,75"/>
                    </svg>
                </Button>
                <canvas id="sound-visualiser" ref={player.sound_visualiser.canvas().clone()} class="blue-border"
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
        let mut player = GLOBAL_PLAYER.get_mut().unwrap_throw(loc!());
        let cb = ctx.link().callback(|()| MainCmd::Resize);
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

        let hint_handler = Rc::clone(&player.hint_handler);
        let cb = JsClosure::<dyn Fn(_)>::new(move |e: PointerEvent| {
            let Some(target) = e.target_dyn_into::<Element>()
                .report_err(loc!()) else {return};
            let [main, aux] = get_hint(target);
            _ = hint_handler.set_hint(&main, &aux).report_err(loc!());
        }).into_js_value().unchecked_into();
        window.set_onpointerover(Some(&cb));

        player.handle_resize().unwrap_throw(loc!());
    }
}

impl Main {
    /// returns `true` if there's an expected change of layout
    #[inline] fn set_param(&mut self, id: ParamId, _value: R64) -> bool {
        if let ParamId::Select = id {
            self.editor_tab_id = 0;
            true
        } else {false}
    }
}

fn main() {
    yew::Renderer::<Main>::new().render();
}
