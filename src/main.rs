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
use sound::SoundType;
use visual::{SoundVisualiser, EditorPlaneHandler, CanvasEvent, HintHandler};
use sequencer::Sequencer;
use utils::{
    JsResultUtils, JsResult,
    MaybeCell, WasmCell,
    window, R64, Pipe, Point};
use web_sys::{
    console::warn_1,
    HtmlCanvasElement, PointerEvent, DragEvent};
use wasm_bindgen::{
    JsCast,
    JsValue,
    closure::Closure as JsClosure};
use js_sys::Function as JsFunction;
use yew::{
    Callback,
    Component,
    Context, Html, html, AttrValue, TargetCast};

use crate::{sound::Sound, utils::ResultToJsResult};

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
                let time: R64 = time.try_into().to_js_result(loc!())?;
                let mut handle = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                let Player{ref mut editor_plane_handler, ref mut sound_visualiser,
                    ref mut sequencer, ref mut hint_handler, ref js_callback} = *handle;

                sequencer.poll(time).add_loc(loc!())?;
                sound_visualiser.poll(sequencer.visualiser()).add_loc(loc!())?;
                editor_plane_handler.poll(sequencer.pattern(), hint_handler).add_loc(loc!())?;
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
            | self.editor_plane_handler.set_param(id, value).add_loc(loc!())?)
    }

    pub fn handle_resize(&mut self) -> JsResult<()> {
        self.editor_plane_handler.handle_resize().add_loc(loc!())?;
        self.sound_visualiser.handle_resize().add_loc(loc!())?;
        Ok(())
    }
}

pub struct Main {
    error_count: usize,
    editor_tab_id: usize
}

#[derive(Debug)]
pub enum MainCmd {
    Hover(PointerEvent),
    Focus(PointerEvent),
    Unfocus(PointerEvent),
    Leave,
    SetParam(ParamId, R64),
    ReportError(JsValue),
    SetHint(AttrValue, AttrValue),
    ClearHint,
    SetTab(usize),
    Resize,
    BlockAddStart(SoundType),
    BlockAddEnd(DragEvent)
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
        Player::init_global().add_loc(loc!()).unwrap_throw(loc!());
        Self{error_count: 0, editor_tab_id: 0}
    }

    fn update(&mut self, _: &Context<Self>, msg: Self::Message) -> bool {
        let on_new_error = |this: &mut Self, err: JsValue| -> bool {
            this.error_count += 1;
            warn_1(&err);
            true
        };

        let err = js_try!{type = !:
            return match msg {
                MainCmd::Hover(e) => {
                    let e = CanvasEvent::try_from(&*e).add_loc(loc!())?;
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    let Player{ref mut editor_plane_handler, ref mut sequencer, ..} = *player;
                    if let Some((id, value)) = editor_plane_handler.handle_hover(Some(e), sequencer.pattern_mut()) {
                        player.set_param(id, value).add_loc(loc!())?
                    } else {false}
                }

                MainCmd::Focus(e) => {
                    e.target_unchecked_into::<HtmlCanvasElement>()
                        .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    let e = CanvasEvent::try_from(&*e).add_loc(loc!())?;
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    let Player{ref mut editor_plane_handler, ref mut sequencer, ..} = *player;
                    if let Some((id, value)) = editor_plane_handler.handle_hover(Some(e), sequencer.pattern_mut()) {
                        player.set_param(id, value).add_loc(loc!())?
                    } else {false}
                }

                MainCmd::Unfocus(e) => {
                    e.target_unchecked_into::<HtmlCanvasElement>()
                        .release_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    let e = CanvasEvent::try_from(&*e).add_loc(loc!())?;
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    let Player{ref mut editor_plane_handler, ref mut sequencer, ..} = *player;
                    if let Some((id, value)) = editor_plane_handler.handle_hover(Some(e), sequencer.pattern_mut()) {
                        player.set_param(id, value).add_loc(loc!())?
                    } else {false}
                }

                MainCmd::Leave => {
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    let Player{ref mut editor_plane_handler, ref mut sequencer, ..} = *player;
                    if let Some((id, value)) = editor_plane_handler.handle_hover(None, sequencer.pattern_mut()) {
                        player.set_param(id, value).add_loc(loc!())?
                    } else {false}
                }

                MainCmd::SetHint(main, aux) =>
                    GLOBAL_PLAYER.get_mut().add_loc(loc!())?
                        .hint_handler.set_hint(&main, &aux).pipe(|_| false),

                MainCmd::ClearHint =>
                    GLOBAL_PLAYER.get_mut().add_loc(loc!())?
                        .hint_handler.clear_hint().pipe(|_| false),

                MainCmd::SetParam(id, value) =>
                    GLOBAL_PLAYER.get_mut().add_loc(loc!())?
                        .set_param(id, value).add_loc(loc!())?,

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
                    GLOBAL_PLAYER.get_mut().add_loc(loc!())?
                        .editor_plane_handler.handle_block_add(Some(decl), None);
                    false
                }

                MainCmd::BlockAddEnd(e) => {
                    let at = Some(Point{x: e.offset_x(), y: e.offset_y()});
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    if let Some((id, val)) = player.editor_plane_handler.handle_block_add(None, at) {
                        player.set_param(id, val).add_loc(loc!())?
                    } else {false}
                }

                MainCmd::ReportError(err) => return on_new_error(self, err)
            }
        };
        on_new_error(self, err.into_err())
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        js_try!{
            let player = GLOBAL_PLAYER.get().add_loc(loc!())?;
            let block = player.editor_plane_handler.selected_element_id()
                .map(|x| (x, unsafe{player.sequencer.pattern().get_unchecked(x)}));
            let hint = &player.hint_handler;

            html! {<>
                <div id="main-panel">
                    <div id="ctrl-panel" class="dark-bg"
                    onpointerover={hint.setter("Settings", block.map_or("General".to_owned(), |(_, x)| x.desc()))}>
                        <div id="hint" class="light-bg"
                        onpointerover={hint.setter("Hint bar", "for useful messages about the app's controls")}>
                            <span id="main-hint" ref={player.hint_handler.main_bar().clone()}/>
                            <br/>
                            <span id="aux-hint" ref={player.hint_handler.aux_bar().clone()}/>
                        </div>
                        if let Some((id, block)) = block {
                            {block.sound.params(id, hint)}
                            <div id="general-ctrl" class="dark-bg">
                                <Button {hint}
                                id={ParamId::Remove(id)}
                                name={"Remove component"}>
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
                                onpointerover={hint.setter("General", "Settings tab")}>
                                    <p onpointerup={ctx.link().callback(|_| MainCmd::SetTab(0))}>{"General"}</p>
                                </div>
                                <div id={(self.editor_tab_id == 1).then_some("selected-tab")}
                                onpointerup={ctx.link().callback(|_| MainCmd::SetTab(1))}
                                onpointerover={hint.setter("Add Block", "Settings tab")}>
                                    <p onpointerup={ctx.link().callback(|_| MainCmd::SetTab(1))}>{"Add Block"}</p>
                                </div>
                            </div>
                            if self.editor_tab_id == 0 {
                                <div id="inputs">
                                    <Slider {hint} key="tmp" name="Tempo"
                                        id={ParamId::Bpm}
                                        min={r64![30.0]} max={r64![240.0]}
                                        postfix="BPM"
                                        initial={player.sequencer.bps() * r64![60.0]}/>
                                    <Slider {hint} key="gain" name="Master gain level"
                                        id={ParamId::MasterGain}
                                        initial={R64::from(player.sequencer.gain())}/>
                                </div>
                            } else if self.editor_tab_id == 1 {
                                <div id="block-add-menu">
                                    {for Sound::TYPES.iter().map(|x| html!{
                                        <div draggable="true"
                                        onpointerover={hint.setter(x.name(), "Click to add block to plane")}
                                        ondragstart={ctx.link().callback(|_| MainCmd::BlockAddStart(*x))}
                                        ondragend={ctx.link().callback(MainCmd::BlockAddEnd)}>
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
                    onpointerout={ctx.link().callback(|_| MainCmd::Leave)}/>
                </div>
                <div id="io-panel" onpointerover={hint.setter("Editor plane settings", "")}>
                    {player.editor_plane_handler.params(hint)}
                    <canvas id="sound-visualiser" ref={player.sound_visualiser.canvas().clone()} class="blue-border"
                    onpointerover={hint.setter("Sound visualiser", "")}/>
                </div>
                if self.error_count > 0 {
                    <div id="error-count">{format!("Errors: {}", self.error_count)}</div>
                }
            </>}
        }.unwrap_throw(loc!())
    }

    fn rendered(&mut self, ctx: &Context<Self>, first_render: bool) {
        if !first_render {return}
        let cb = ctx.link().callback(|()| MainCmd::Resize);
        let cb = JsClosure::<dyn Fn()>::new(move || cb.emit(()))
            .into_js_value().unchecked_into();
        window().set_onresize(Some(&cb));
        GLOBAL_PLAYER.get_mut().unwrap_throw(loc!())
            .handle_resize().unwrap_throw(loc!());
    }
}

fn main() {
    yew::set_event_bubbling(false);
    yew::Renderer::<Main>::new().render();
}
