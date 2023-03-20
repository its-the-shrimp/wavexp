#![feature(result_option_inspect)]
#![feature(try_blocks)]
#![feature(never_type)]
#![feature(unwrap_infallible)]
mod render;
mod utils;
mod input;
mod sound;
mod draggable;
use render::Renderer;
use sound::{
    SoundGen,
    Sound};
use utils::{
    HtmlCanvasExt, HtmlDocumentExt,
    SliceExt, BoolExt, VecExt,
    JsResultUtils, OptionToJsResult, JsResult,
    Point,
    MaybeCell, WasmCell,
    document, window};
use web_sys::{
    console::warn_1,
    Path2d as JsPath2d,
    HtmlCanvasElement,
    ImageData as JsImageData,
    AnalyserNode, AudioContext, HtmlElement, PointerEvent};
use wasm_bindgen::{
    JsValue,
    Clamped as JsClamped,
    closure::Closure as JsClosure};
use wasm_bindgen::JsCast;
use js_sys::{
    Array as JsArray,
    Function as JsFunction};
use yew::{
    Callback,
    Component,
    Context};
use std::rc::Rc;

struct AnimationCtx {
    analyser: Rc<AnalyserNode>,
    graph_canvas: HtmlCanvasElement,
    sound_visualiser_canvas: HtmlCanvasElement,
    renderer: Renderer,
    graph: JsPath2d,
    solid_line: JsValue,
    dotted_line: JsValue,
    graph_in_span: u32,
    graph_span: u32,
    pbar_start: f64,
    js_callback: JsFunction
}

static ANIMATION_CTX: WasmCell<MaybeCell<AnimationCtx>> = WasmCell::new(MaybeCell::new());

fn start_animation_loop() -> JsResult<()> {
    fn render(time: f64) {
        _ = js_try!{type = !:
                let mut handle = ANIMATION_CTX.get_mut()?;
                let AnimationCtx{ref analyser, ref mut renderer, 
                    ref graph_canvas, ref sound_visualiser_canvas,
                    ref graph, ref solid_line, ref dotted_line,
                    ref graph_in_span, ref graph_span, ref mut pbar_start,
                    ref js_callback} = *handle;
                let graph_out_span = (graph_span - graph_in_span) as f64;

                let err1 = js_try!{
                    let buf = renderer.set_size(sound_visualiser_canvas.width() as usize, sound_visualiser_canvas.height() as usize)
                        .get_in_buffer();
                    analyser.get_byte_frequency_data(buf);
                    sound_visualiser_canvas.get_2d_context()?.put_image_data(
                        &JsImageData::new_with_u8_clamped_array(
                            JsClamped(renderer.graph().get_out_bytes()), sound_visualiser_canvas.width())?,
                        0.0, 0.0)?;
                }.explain_err("re-rendering the sound visualisation");

                let err2 = js_try!{
                    let (w, h, ctx) = (graph_canvas.width().into(), graph_canvas.height().into(), graph_canvas.get_2d_context()?);
                    ctx.set_fill_style(&"#181818".into());
                    ctx.fill_rect(0.0, 0.0, w, h);
                    if *graph_span > 0 {
                        ctx.set_line_width(3.0);
                        ctx.set_stroke_style(&"#0069E1".into());
                        ctx.stroke_with_path(graph);
                        if !pbar_start.is_nan() {
                            if pbar_start.is_infinite() {
                                *pbar_start = time.copysign(*pbar_start);
                            }
                            if pbar_start.is_sign_negative() && (time + *pbar_start > graph_out_span) {
                                *pbar_start = f64::NAN;
                            } else {
                                let x = if pbar_start.is_sign_positive() {
                                    (time - *pbar_start).min(*graph_in_span as f64)
                                } else {
                                    time + *pbar_start + *graph_in_span as f64
                                } / *graph_span as f64 * w;
                                ctx.set_line_dash(dotted_line)?;
                                ctx.set_line_width(1.0);
                                ctx.set_line_dash_offset(time / 100.0);
                                ctx.begin_path();
                                ctx.move_to(x, 0.0);
                                ctx.line_to(x, h);
                                ctx.stroke();
                                ctx.set_line_dash(solid_line)?;
                            }
                        }
                    }
                }.explain_err("re-rendering the graphical representation of a sound element's parameters");

                let err3 = window().request_animation_frame(js_callback)
                    .explain_err("re-registering the animation callback");

                err1.and(err2).and(err3)?;
                return
        }.report_err("rendering frame-by-frame animation");
    }

    ANIMATION_CTX.get_mut()
        .explain_err("starting the animation loop")?
        .js_callback = JsClosure::<dyn Fn(f64)>::new(render)
            .into_js_value().unchecked_into::<JsFunction>();
    render(0.0);
    Ok(())
}

pub struct Main {
    analyser: Rc<AnalyserNode>,
    player: AudioContext,
    selected_comp: Option<usize>,
    focused_comp: Option<usize>,
    hovered_comp: Option<usize>,
    error_count: usize,
    plane_moving: bool,
    plane_offset: Point,
    sound_comps: Vec<SoundGen>,
    connections: Vec<Vec<usize>>,
    graph_canvas: HtmlCanvasElement,
    sound_visualiser_canvas: HtmlCanvasElement,
    editor_plane_canvas: HtmlCanvasElement,
    help_msg_bar: HtmlElement,
}

fn traverse<S: Clone>(conns: &[Vec<usize>], src_id: usize, state: S, f: &mut impl FnMut(S, usize, usize) -> JsResult<S>) 
-> JsResult<()> {
    let dsts = conns.get_or_js_error(src_id, "element #", " not found")?;
    for dst_id in dsts.iter() {
        let new_state = f(state.clone(), src_id, *dst_id)?;
        traverse(conns, *dst_id, new_state, f)?;
    }
    Ok(())
}

#[derive(Debug)]
pub enum MainCmd {
    Drag(PointerEvent),
    Focus(PointerEvent),
    Unfocus(PointerEvent),
    LeavePlane,
    SetDesc(String),
    RemoveDesc,
    SetParam(usize, usize, f64),
    TryConnect(usize, Point),
    Select(Option<usize>),
    ReportError(JsValue)
}

static mut MAINCMD_SENDER: Option<Callback<MainCmd>> = None;
impl MainCmd {
    #[inline] pub fn send(self) {
        unsafe{MAINCMD_SENDER.as_ref().unwrap_unchecked()}.emit(self)
    }
}

impl Main {
    const DEF_HELP_MSG: &'static str = "Hover over an element to get help";

    fn set_desc(&self, desc: &str) {
        self.help_msg_bar.set_inner_text(&format!("{:1$}", desc, Self::DEF_HELP_MSG.len()))
    }

    fn remove_desc(&self) {
        self.help_msg_bar.set_inner_text(Self::DEF_HELP_MSG)
    }
}

impl Component for Main {
    type Message = MainCmd;
    type Properties = ();

    fn create(ctx: &Context<Self>) -> Self {
        *unsafe{&mut MAINCMD_SENDER} = Some(ctx.link().callback(|msg| msg));

        js_try!{
            let player = AudioContext::new()?;
            let analyser = Rc::new(AnalyserNode::new(&player)?);
            let sound_comps = vec![
                SoundGen::new_input(0, Point{x:300, y:500}),
                SoundGen::new_wave(1, Point{x:300, y:350}),
                SoundGen::new_envelope(2, Point{x:500, y:350}),
                SoundGen::new_output(3, Point{x:750, y:350}, analyser.clone())];
            analyser.connect_with_audio_node(&player.destination())?;
            let help_msg_bar: HtmlElement = document().create_element("div")
                .expect_throw("creating #help-msg element").unchecked_into();
            help_msg_bar.set_id("help-msg");
            help_msg_bar.set_inner_text(Self::DEF_HELP_MSG);

            Self {analyser, sound_comps, player, help_msg_bar,
                selected_comp: None, focused_comp: None, hovered_comp: None,
                plane_offset: Point::ZERO, plane_moving: false,
                graph_canvas: JsValue::UNDEFINED.unchecked_into(),
                sound_visualiser_canvas: JsValue::UNDEFINED.unchecked_into(),
                editor_plane_canvas: JsValue::UNDEFINED.unchecked_into(),
                error_count: 0, connections: vec![vec![], vec![], vec![], vec![]]}
        }.expect_throw("initialising the main component")
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        let on_new_error = |this: &mut Self, err: JsValue| -> bool {
            this.error_count += 1;
            warn_1(&err);
            true
        };

        let err = js_try!{type = !:
            let mut needs_plane_rerender = false;
            let mut needs_html_rerender = false;
            match msg {
                MainCmd::Drag(e) => js_try!{
                    if let Some(id) = self.focused_comp {
                        needs_plane_rerender = true;
                        let comp = self.sound_comps.get_mut_or_js_error(id, "sound element #", " not found")?;
                        if let Some(msg) = comp.handle_movement(Point{x: e.x(), y: e.y()} + self.plane_offset, false) {
                            needs_html_rerender = self.update(ctx, msg)
                        }
                    } else if self.plane_moving {
                        needs_plane_rerender = true;
                        self.plane_offset -= Point{x: e.movement_x(), y: 0};
                    } else {
                        if let Some(id) = self.hovered_comp {
                            let point = Point{x: e.x(), y: e.y()} + self.plane_offset;
                            if !self.sound_comps.get_or_js_error(id, "sound element #", " not found")?.contains(point) {
                                self.hovered_comp = None;
                                self.remove_desc();
                            }
                        } if self.hovered_comp.is_none() {
                            let point = Point{x: e.x(), y: e.y()} + self.plane_offset;
                            if let Some(comp) = self.sound_comps.iter().find(|x| x.contains(point)) {
                                self.hovered_comp = Some(comp.id);
                                self.set_desc(comp.name());
                            }
                        }
                    }
                }.explain_err("handling `MainCmd::Drag` message")?,

                MainCmd::Focus(e) => js_try!{
                    self.editor_plane_canvas.set_pointer_capture(e.pointer_id())?;
                    needs_plane_rerender = true;
                    let point = Point{x: e.x(), y: e.y()} + self.plane_offset;
                    self.focused_comp = self.sound_comps.iter()
                        .position(|c| c.contains(point));
                    if let Some(comp) = self.focused_comp {
                        let comp = unsafe {self.sound_comps.get_unchecked_mut(comp)};
                        if let Some(msg) = comp.handle_movement(Point{x: e.x(), y: e.y()} + self.plane_offset, false) {
                            needs_html_rerender = self.update(ctx, msg);
                        }
                    } else {
                        self.plane_moving = true;
                        self.set_desc("Dragging the plane");
                    }
                }.explain_err("handling `MainCmd::Focus` message")?,

                MainCmd::Unfocus(e) => js_try!{
                    self.editor_plane_canvas.release_pointer_capture(e.pointer_id())?;
                    needs_plane_rerender = true;
                    if let Some(id) = self.focused_comp.take() {
                        let comp = self.sound_comps.get_mut_or_js_error(id, "sound functor #", " not found")?;
                        let name = comp.name();
                        if let Some(msg) = comp.handle_movement(Point{x:e.x(), y:e.y()} + self.plane_offset, true) {
                            needs_html_rerender = self.update(ctx, msg);
                        }
                        self.set_desc(name);
                    } else {
                        ctx.link().send_message(MainCmd::Select(None));
                        self.plane_moving = false;
                        self.remove_desc();
                    }
                }.explain_err("handling `MainCmd::Unfocus` message")?,

                MainCmd::LeavePlane => js_try!{
                    self.hovered_comp = None;
                    self.remove_desc();
                }.explain_err("handling `MainCmd::LeavePlane` message")?,

                MainCmd::SetDesc(value) => self.set_desc(&value),

                MainCmd::RemoveDesc => self.remove_desc(),

                MainCmd::SetParam(comp_id, param_id, value) => js_try!{
                    if comp_id == self.sound_comps.len() {
                        js_try!{
                            match param_id {
                                0 => { // starting to play the sounds
                                    ANIMATION_CTX.get_mut()?.pbar_start = value;
                                    let sound = value.is_sign_positive().choose(Sound::InputFreq(0.0), Sound::End);
                                    traverse(&self.connections, 0, sound, &mut |sound, _, dst_id| {
                                        self.sound_comps.get_mut_or_js_error(dst_id, "sound element #", " not found")?
                                            .transform(&self.player, sound)
                                    })?;
                                    return false
                                }

                                1 => { // removing all outward connections of the selected component
                                    if value.is_sign_positive() {return false}
                                    let selected_comp_id = self.selected_comp
                                        .to_js_result("no sound element selected")?;
                                    self.connections.get_mut_or_js_error(selected_comp_id, "sound element #", " not found")?
                                        .clear();
                                }

                                2 => { // removing the selected component
                                    if value.is_sign_positive() {return false}
                                    let selected_comp_id = self.selected_comp
                                        .to_js_result("no sound element selected")?;
                                    self.sound_comps.try_swap_remove(selected_comp_id)
                                        .to_js_result_with(|| format!("sound element #{} not found", selected_comp_id))?;
                                    self.connections.try_swap_remove(selected_comp_id)
                                        .to_js_result_with(|| format!("sound element #{} not found", selected_comp_id))?;
                                    let len = self.connections.len();
                                    for conns in self.connections.iter_mut() {
                                        conns.retain_mut(|x| {
                                            if *x == selected_comp_id {return false}
                                            if *x == len {*x = selected_comp_id}
                                            true
                                        });
                                    }
                                    unsafe{self.sound_comps.get_unchecked_mut(selected_comp_id)}.id = selected_comp_id;
                                }

                                _ => JsResult::<!>::Err(format!("unknown parameter ID: {}", param_id).into())?
                            }
                            needs_html_rerender = true;
                            needs_plane_rerender = true;
                        }.explain_err("handling commands on the selected sound component")?;
                    } else {
                        let comp = self.sound_comps
                            .get_mut_or_js_error(comp_id, "sound component #", " not found")?;
                        needs_html_rerender = comp.set_param(param_id, value)?;
                        let (graph, graph_in_span, graph_span)
                            = comp.graph(self.graph_canvas.width().into(), self.graph_canvas.height().into())?;
                        let mut ctx = ANIMATION_CTX.get_mut()?;
                        ctx.graph = graph;
                        ctx.graph_in_span = graph_in_span;
                        ctx.graph_span = graph_span;
                    }
                }.explain_err("handling `MainCmd::SetParam` message")?,

                MainCmd::TryConnect(src_id, dst_pos) => js_try!{
                    if let Some(dst) = self.sound_comps.iter().find(|x| x.contains(dst_pos)) {
                        if dst.id != src_id && dst.backwardable().is_some() {
                            let src_conns = self.connections
                                .get_mut_or_js_error(src_id, "sound element #", " not found")?;
                            if !src_conns.contains(&dst.id) {src_conns.push(dst.id)}
                        }
                    } else {
                        self.remove_desc()
                    }
                    needs_plane_rerender = true;
                }.explain_err("handling `MainCmd::TryConnect` message")?,

                MainCmd::Select(id) => js_try!{type = !:
                    self.selected_comp = id.filter(|id| Some(id) != self.selected_comp.as_ref());
                    if let Some(id) = self.selected_comp {
                        let (graph, graph_in_span, graph_span) = self.sound_comps
                            .get_or_js_error(id, "sound functor #", " not found")?
                            .graph(self.graph_canvas.width().into(), self.graph_canvas.height().into())?;
                        let mut ctx = ANIMATION_CTX.get_mut()?;
                        ctx.graph = graph;
                        ctx.graph_in_span = graph_in_span;
                        ctx.graph_span = graph_span;
                    } else {
                        ANIMATION_CTX.get_mut()?.graph_span = 0}
                    return true
                }.explain_err("handling `MainCmd::Select` message")?,

                MainCmd::ReportError(err) => return on_new_error(self, err)
            };

            if needs_plane_rerender {
                self.render_editor_plane()?;
            }
            return needs_html_rerender
        };
        on_new_error(self, err.into_err())
    }

    fn view(&self, ctx: &Context<Self>) -> yew::Html {
        let comp = self.selected_comp.and_then(|i| self.sound_comps.get(i));

        return yew::html! {<>
            <canvas width="100%" height="100%" id="plane"
            onpointerdown={ctx.link().callback(MainCmd::Focus)}
            onpointerup={ctx.link().callback(MainCmd::Unfocus)}
            onpointermove={ctx.link().callback(MainCmd::Drag)}
            onpointerleave={ctx.link().callback(|_| MainCmd::LeavePlane)}/>
            <div id="ctrl-panel">
                {yew::Html::VRef(self.help_msg_bar.clone().into())}
                {comp.map(|x| x.params())}
                <canvas id="graph" class="visual"
                hidden={comp.map(|x| !x.graphable()).unwrap_or(true)}/>
                if comp.is_some() {
                    <div id="general-ctrl">
                        <input::Button
                        component_id={self.sound_comps.len()} id={1}
                        desc={"Disconnect component"}>
                            <svg viewBox="0 0 100 100">
                                <polygon points="10,40 10,60 40,60 30,40"/>
                                <polygon points="30,20 60,80 70,80 40,20"/>
                                <polygon points="50,40 80,40 80,60 60,60"/>
                            </svg>
                        </input::Button>
                        <input::Button
                        component_id={self.sound_comps.len()} id={2}
                        desc={"Remove component"}>
                            <svg viewBox="0 0 100 100">
                                <polygon points="27,35 35,27 50,42 65,27 73,35 58,50 73,65 65,73 50,58 35,73 27,65 42,50"/>
                            </svg>
                        </input::Button>
                    </div>
                }
            </div>
            <div id="visuals">
                <canvas id="sound-visualiser" class="visual"/>
                <input::Button
                component_id={self.sound_comps.len()} id={0}
                desc={"Play"}>
                    <svg viewBox="0 0 100 100" height="100%">
                        <polygon points="25,25 75,50 25,75"/>
                    </svg>
                </input::Button>
            </div>
            if self.error_count > 0 {
                <div id="error-count">{format!("Errors: {}", self.error_count)}</div>
            }
        </>}
    }

    fn rendered(&mut self, _: &Context<Self>, first_render: bool) {
        _ = js_try!{type = !:
            return if first_render {
                self.graph_canvas = document().element_dyn_into("graph")?;
                self.graph_canvas.set_hidden(false);
                self.graph_canvas.sync();
                self.graph_canvas.set_hidden(true);
                self.sound_visualiser_canvas = document().element_dyn_into("sound-visualiser")?;
                self.sound_visualiser_canvas.sync();
                self.help_msg_bar = document().element_dyn_into("help-msg")?;
                js_try!{
                    self.editor_plane_canvas = document().element_dyn_into("plane")?;
                    let body = document().body().to_js_result("<body> not found")?;
                    self.editor_plane_canvas.set_width(body.client_width() as u32);
                    self.editor_plane_canvas.set_height(body.client_height() as u32);
                }.explain_err("initialising the editor plane")?;
                js_try!{
                    let ctx = self.editor_plane_canvas.get_2d_context()?;
                    ctx.set_stroke_style(&"#0069E1".into());
                    ctx.set_fill_style(&"#232328".into());
                    ctx.set_line_width(3.0);
                    self.render_editor_plane()?;
                }.explain_err("drawing the editor plane for the first time")?;

                ANIMATION_CTX.set(AnimationCtx {
                    analyser: self.analyser.clone(), renderer: Renderer::new(),
                    sound_visualiser_canvas: self.sound_visualiser_canvas.clone(),
                    graph_canvas: self.graph_canvas.clone(),
                    solid_line: JsArray::new().into(),
                    dotted_line: JsArray::of2(&(10.0).into(), &(10.0).into()).into(),
                    graph: JsPath2d::new()?,
                    graph_in_span: 0,
                    graph_span: 0,
                    pbar_start: f64::NAN,
                    js_callback: Default::default() // initialized later in `start_animation_loop`
                })?;
                start_animation_loop()?;
            }
        }.expect_throw("rendering the main element");
    }
}

impl Main {
    fn render_editor_plane(&self) -> JsResult<()> {
        let ctx = self.editor_plane_canvas.get_2d_context()?;
        ctx.fill_rect(0.0, 0.0,
            self.editor_plane_canvas.width().into(),
            self.editor_plane_canvas.height().into());
        ctx.begin_path();
        self.sound_comps.iter()
            .for_each(|c| c.draw(&ctx, self.plane_offset));
        for (src_id, dsts) in self.connections.iter().enumerate() {
            let Some(src) = self.sound_comps
                .get_or_js_error(src_id, "sound element #", " not found")?
                .forwardable().map(|x| x - self.plane_offset) else {continue};
            for &dst_id in dsts.iter() {
                let dst = self.sound_comps
                    .get_or_js_error(dst_id, "sound element #", " not found")?
                    .backwardable().to_js_result("un-backwardable sound element was forwarded to")?
                    - self.plane_offset;
                ctx.move_to(src.x as f64, src.y as f64);
                ctx.line_to(dst.x as f64, dst.y as f64);
            }
        }
        Ok(ctx.stroke())
    }
}

fn main() {
    yew::Renderer::<Main>::new().render();
}
