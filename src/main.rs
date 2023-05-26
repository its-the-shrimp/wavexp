#![feature(try_blocks)]
#![feature(never_type)]
#![feature(unwrap_infallible)]
#![feature(const_slice_index)]
#![allow(clippy::unit_arg)]
#![allow(clippy::option_map_unit_fn)]

mod visual;
mod utils;
mod input;
mod sound;
mod sequencer;
use std::{
    fmt::Debug,
    rc::Rc};
use input::{ParamId, Button};
use visual::{GraphHandler, SoundVisualiser, EditorPlaneHandler, CanvasEvent, HintHandler};
use sequencer::Sequencer;
use sound::{Sound, Note};
use utils::{
    JsResultUtils, OptionExt, JsResult,
    Point,
    MaybeCell, WasmCell,
    document, window, R64, Pipe};
use web_sys::{
    console::warn_1,
    HtmlCanvasElement, PointerEvent};
use wasm_bindgen::{
    JsCast,
    JsValue,
    closure::Closure as JsClosure};
use js_sys::Function as JsFunction;
use yew::{
    virtual_dom::VList,
    Callback,
    Component,
    Context, Html, html, AttrValue, TargetCast};

/// responsible for playing sounds and frame-by-frame animations
struct Player {
    pub sound_visualiser: SoundVisualiser,
    pub graph_handler: GraphHandler,
    pub editor_plane_handler: EditorPlaneHandler,
    pub sequencer: Sequencer,
    pub hint_handler: Rc<HintHandler>,
    pub js_callback: JsFunction
}

static GLOBAL_PLAYER: WasmCell<MaybeCell<Player>> = WasmCell::new(MaybeCell::new());

impl Player {
    fn init_global() -> JsResult<()> {
        fn render(time: f64) {
            let Ok(time) = R64::try_from(time).map_err(|x| x.to_string().into()).report_err(loc!())
                else {return};
            let Ok(mut handle) = GLOBAL_PLAYER.get_mut().report_err(loc!())
                else {return};
            let Player{ref mut editor_plane_handler, ref mut graph_handler, ref mut sound_visualiser,
                ref mut sequencer, ref mut hint_handler, ref js_callback} = *handle;

            _ = sequencer.poll(time).report_err(loc!());
            _ = sound_visualiser.poll(sequencer.visualiser()).report_err(loc!());
            let sel = editor_plane_handler.selected_element_id()
                .map(|x| unsafe{sequencer.elements_mut().get_unchecked_mut(x)});
            _ = graph_handler.poll(sel).report_err(loc!());
            _ = editor_plane_handler
                .poll(sequencer.elements(), sequencer.connections(), hint_handler).report_err(loc!());
            _ = window().request_animation_frame(js_callback).report_err(loc!());
        }

        let js_callback = &GLOBAL_PLAYER.set(Self{
            graph_handler: GraphHandler::new().add_loc(loc!())?,
            sound_visualiser: SoundVisualiser::new().add_loc(loc!())?,
            editor_plane_handler: EditorPlaneHandler::new().add_loc(loc!())?,
            sequencer: Sequencer::new().add_loc(loc!())?,
            hint_handler: Rc::new(HintHandler::default()),
            js_callback: JsClosure::<dyn Fn(f64)>::new(render).into_js_value().unchecked_into()
        }).add_loc(loc!())?.js_callback;

        window().request_animation_frame(js_callback).add_loc(loc!()).map(|_|())
    }

    pub fn set_param(&mut self, id: ParamId, value: R64) -> JsResult<bool> {
        Ok(self.sequencer.set_param(id, value).add_loc(loc!())?
            | self.editor_plane_handler.set_param(id, value).add_loc(loc!())?
            | self.graph_handler.set_param(id, value))
    }
}

pub struct Main {
    error_count: usize,
}

#[derive(Debug)]
pub enum MainCmd {
    Hover(PointerEvent),
    Focus(PointerEvent),
    Unfocus(PointerEvent),
    HoverGraph(PointerEvent),
    FocusGraph(PointerEvent),
    UnfocusGraph(PointerEvent),
    SetParam(ParamId, R64),
    ReportError(JsValue),
    SetHint(AttrValue, AttrValue),
    ClearHint
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
        ctx.link().send_message(ParamId::Add(SoundGen::new_wave, Point{x: 350, y: 441}));
        ctx.link().send_message(ParamId::Add(SoundGen::new_envelope, Point{x: 550, y: 441}));
        ctx.link().send_message(ParamId::Add(SoundGen::new_pattern, Point{x: 450, y: 278}));
        Self{error_count: 0}
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
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    let e = CanvasEvent::try_from(&*e).add_loc(loc!())?;
                    if let Some((id, value)) = player.editor_plane_handler.set_event(Some(e)) {
                        player.set_param(id, value).add_loc(loc!())?;
                    }
                    false
                }

                MainCmd::HoverGraph(e) => {
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    let e = CanvasEvent::try_from(&*e).add_loc(loc!())?;
                    player.graph_handler.set_event(e);
                    false
                }

                MainCmd::Focus(e) => {
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    e.target_unchecked_into::<HtmlCanvasElement>()
                        .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    let e = CanvasEvent::try_from(&*e).add_loc(loc!())?;
                    if let Some((id, value)) = player.editor_plane_handler.set_event(Some(e)) {
                        player.set_param(id, value).add_loc(loc!())?
                    } else {false}
                }

                MainCmd::Unfocus(e) => {
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    e.target_unchecked_into::<HtmlCanvasElement>()
                        .release_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    let e = CanvasEvent::try_from(&*e).add_loc(loc!())?;
                    if let Some((id, value)) = player.editor_plane_handler.set_event(Some(e)) {
                        player.set_param(id, value).add_loc(loc!())?
                    } else {false}
                }

                MainCmd::FocusGraph(e) => {
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    e.target_unchecked_into::<HtmlCanvasElement>()
                        .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    let e = CanvasEvent::try_from(&*e).add_loc(loc!())?;
                    player.graph_handler.set_event(e);
                    false
                }

                MainCmd::UnfocusGraph(e) => {
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    e.target_unchecked_into::<HtmlCanvasElement>()
                        .release_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    let e = CanvasEvent::try_from(&*e).add_loc(loc!())?;
                    player.graph_handler.set_event(e);
                    false
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

                MainCmd::ReportError(err) => return on_new_error(self, err)
            }
        };
        on_new_error(self, err.into_err())
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        fn piano_roll(hint: &Rc<HintHandler>) -> Html {
            let mut list = VList::new();
            list.reserve_exact(Note::ALL.len() + 1);
            const N_DIATONIC_NOTES: f64 = Note::N_OCTAVES as f64 * 7.0;
            let width: Rc<str> = format!("{:.2}%", 100.0 / N_DIATONIC_NOTES).into();

            for note in Note::ALL.iter().filter(|x| !x.is_sharp()) {
                let x = note.diatonic_index() as f64 / (N_DIATONIC_NOTES / 100.0);
                list.add_child(html!{
                    <Button svg={true} class="piano-note" {hint}
                     id={ParamId::Play(*note)}
                     name={format!("Play the {} note", note.name())}>
                        <rect width={width.clone()} height="100%" 
                         x={format!("{:.2}%", x)} y="0"/>
                    </Button>});
            }

            for note in Note::ALL.iter().filter(|x| x.is_sharp()) {
                let x = (note.diatonic_index() as f64 + 0.5) / (N_DIATONIC_NOTES / 100.0);
                list.add_child(html!{
                    <Button svg={true} class="piano-note sharp-note" {hint}
                     id={ParamId::Play(*note)}
                     name={format!("Note {}", note.name())}>
                        <rect width={width.clone()} height="50%" 
                         x={format!("{:.2}%", x)} y="0"/>
                    </Button>});
            }

            list.add_child(html!{<line class="piano-note" x1="100%" x2="100%" y2="100%" transform="translate(-4.5, 0)"/>});
            Html::VList(list)
        }

        fn hint_setter<T>(handler: &Rc<HintHandler>, main: impl AsRef<str>, aux: impl AsRef<str>) -> impl Fn(T) {
            let handler = handler.clone();
            move |_| _ = handler.set_hint(main.as_ref(), aux.as_ref()).report_err(loc!())
        }

        js_try!{
            let player = GLOBAL_PLAYER.get().add_loc(loc!())?;
            let comp_id = player.editor_plane_handler.selected_element_id();
            let hint = &player.hint_handler;

            // TODO: stop hint bar from stretching out because of a long hint
            html! {<>
                <canvas ref={player.editor_plane_handler.canvas().clone()} width="100%" height="100%"
                onpointerdown={ctx.link().callback(MainCmd::Focus)}
                onpointerup={ctx.link().callback(MainCmd::Unfocus)}
                onpointermove={ctx.link().callback(MainCmd::Hover)}/>
                <div id="ctrl-panel" class="dark-bg">
                    <div id="hint" class="light-bg">
                        <span id="main-hint" ref={player.hint_handler.main_bar().clone()}/>
                        <br/>
                        <span id="aux-hint" ref={player.hint_handler.aux_bar().clone()}/>
                    </div>
                    {comp_id.map(|x| unsafe{player.sequencer.elements().get_unchecked(x).params(x, hint)})}
                    <canvas ref={player.graph_handler.canvas().clone()} class="blue-border" height=0
                    onpointerdown={ctx.link().callback(MainCmd::FocusGraph)}
                    onpointerup={ctx.link().callback(MainCmd::UnfocusGraph)}
                    onpointermove={ctx.link().callback(MainCmd::HoverGraph)}/>
                    if let Some(comp_id) = comp_id {
                        <div id="general-ctrl" class="dark-bg">
                            <Button {hint}
                            id={ParamId::Disconnect(comp_id)}
                            name={"Disconnect component"}>
                                <svg viewBox="0 0 100 100">
                                    <polygon points="10,40 10,60 40,60 30,40"/>
                                    <polygon points="30,20 60,80 70,80 40,20"/>
                                    <polygon points="50,40 80,40 80,60 60,60"/>
                                </svg>
                            </Button>
                            <Button {hint}
                            id={ParamId::Remove(comp_id)}
                            name={"Remove component"}>
                                <svg viewBox="0 0 100 100">
                                    <polygon points="27,35 35,27 50,42 65,27 73,35 58,50 73,65 65,73 50,58 35,73 27,65 42,50"/>
                                </svg>
                            </Button>
                        </div>
                    }
                </div>
                <div id="io-panel"
                onpointerenter={hint_setter(hint, "MIDI board", "make the input and output nodes connected and click one of the keys")}>
                    <svg id="piano" viewBox={format!("-3 0 {} 100", Note::ALL.len() * 20 - 3)}>
                        {piano_roll(hint)}
                    </svg>
                    <canvas id="sound-visualiser" ref={player.sound_visualiser.canvas().clone()} class="blue-border"
                    onpointerenter={hint_setter(hint, "Sound visualiser", "")}/>
                </div>
                if self.error_count > 0 {
                    <div id="error-count">{format!("Errors: {}", self.error_count)}</div>
                }
            </>}
        }.unwrap_throw(loc!())
    }
}

fn main() {
    yew::Renderer::<Main>::new().render();
}
