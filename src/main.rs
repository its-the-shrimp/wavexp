#![feature(get_many_mut)]
#![feature(is_some_and)]
#![feature(result_option_inspect)]
#![feature(try_blocks)]
#![feature(never_type)]
#![feature(unwrap_infallible)]
mod render;
mod utils;
mod input;
mod sound;
mod draggable;
use utils::{JsResultUtils, HtmlCanvasExt, HtmlDocumentExt, SliceExt, OptionToJsResult};
use web_sys;
use js_sys;
use wasm_bindgen;
use wasm_bindgen::JsCast;
use std::cmp::Ordering;
use std::rc::Rc;

struct AnimationCtx {
    analyser: Rc<web_sys::AnalyserNode>,
    graph_canvas: web_sys::HtmlCanvasElement,
    sound_visualiser_canvas: web_sys::HtmlCanvasElement,
    renderer: render::Renderer,
    graph: web_sys::Path2d,
    solid_line: wasm_bindgen::JsValue,
    dotted_line: wasm_bindgen::JsValue,
    graph_in_span: f64,
    graph_span: f64,
    pbar_start: f64,
    js_callback: js_sys::Function
}

static ANIMATION_CTX: utils::WasmCell<utils::MaybeCell<AnimationCtx>> = utils::WasmCell::new(utils::MaybeCell::new());

fn start_animation_loop() -> utils::JsResult<()> {
    fn render(time: f64) {
        _ = utils::js_try!{type = !:
                let mut handle = ANIMATION_CTX.get_mut()?;
                let AnimationCtx{ref analyser, ref mut renderer, 
                    ref graph_canvas, ref sound_visualiser_canvas,
                    ref graph, ref solid_line, ref dotted_line,
                    ref graph_in_span, ref graph_span, ref mut pbar_start,
                    ref js_callback} = *handle;
                let graph_out_span = graph_span - graph_in_span;

                let err1 = utils::js_try!{
                    let buf = renderer.set_size(sound_visualiser_canvas.width() as usize, sound_visualiser_canvas.height() as usize)
                        .get_in_buffer();
                    analyser.get_byte_frequency_data(buf);
                    sound_visualiser_canvas.get_2d_context()?.put_image_data(
                        &web_sys::ImageData::new_with_u8_clamped_array(
                            wasm_bindgen::Clamped(renderer.graph().get_out_bytes()), sound_visualiser_canvas.width())?,
                        0.0, 0.0)?;
                }.explain_err("re-rendering the sound visualisation");

                let err2 = utils::js_try!{
                    let (w, h, ctx) = (graph_canvas.width().into(), graph_canvas.height().into(), graph_canvas.get_2d_context()?);
                    ctx.set_fill_style(&"#181818".into());
                    ctx.fill_rect(0.0, 0.0, w, h);
                    if graph_span.is_finite() {
                        ctx.set_line_width(3.0);
                        ctx.set_stroke_style(&"#0069E1".into());
                        ctx.stroke_with_path(graph);
                        if !pbar_start.is_nan() {
                            if pbar_start.is_infinite() {
                                *pbar_start = time.copysign(*pbar_start) / 1000.0}

                            if pbar_start.is_sign_negative() && (time / 1000.0 + *pbar_start > graph_out_span) {
                                *pbar_start = f64::NAN;
                            } else {
                                let x = if pbar_start.is_sign_positive() {
                                    (time / 1000.0 - *pbar_start).min(*graph_in_span)
                                } else {
                                    time / 1000.0 + *pbar_start + *graph_in_span
                                } / *graph_span * w;
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
                
                let err3 = utils::window().request_animation_frame(js_callback)
                    .explain_err("re-registering the animation callback");

                err1.and(err2).and(err3)?;
                return
        }.report_err("rendering frame-by-frame animation");
    }

    ANIMATION_CTX.get_mut()
        .explain_err("starting the animation loop")?
        .js_callback = wasm_bindgen::closure::Closure::<dyn Fn(f64)>::new(render)
            .into_js_value().unchecked_into::<js_sys::Function>();
    render(0.0);
    Ok(())
}

pub struct Main {
    analyser: Rc<web_sys::AnalyserNode>,
    player: web_sys::AudioContext,
    selected_comp: Option<usize>,
    focused_comp: Option<usize>,
    hovered_comp: Option<usize>,
    error_count: usize,
    plane_moving: bool,
    plane_offset: utils::Point,
    sound_comps: Vec<sound::SoundFunctor>,
    graph_canvas: web_sys::HtmlCanvasElement,
    sound_visualiser_canvas: web_sys::HtmlCanvasElement,
    editor_plane_canvas: web_sys::HtmlCanvasElement,
    release_times: Vec<(usize, f64)>
}

#[derive(Debug)]
pub enum MainCmd {
    Drag(web_sys::PointerEvent),
    Focus(web_sys::PointerEvent),
    Unfocus(web_sys::PointerEvent),
    LeavePlane,
    SetDesc(String),
    RemoveDesc,
    SetParam(usize, usize, f64),
    SetGlobalReleaseTime(usize, f64),
    TryConnect(usize, utils::Point),
    Select(usize),
    Start, 
    End,
    ReportError(wasm_bindgen::JsValue)
}

static mut MAINCMD_SENDER: Option<yew::Callback<MainCmd>> = None;
impl MainCmd {
    #[inline] pub fn send(self) {
        unsafe{MAINCMD_SENDER.as_ref().unwrap_unchecked()}.emit(self)
    }
}

impl Main {
    const DEF_HELP_MSG: &'static str = "Hover over an element to get help";

    pub fn set_desc(desc: &str) -> utils::JsResult<()> {
        utils::document().element_dyn_into::<web_sys::HtmlElement>("help-msg")?
            .set_inner_text(&format!("{:1$}", desc, Self::DEF_HELP_MSG.len()));
        Ok(())
    }

    pub fn remove_desc() -> utils::JsResult<()> {
        utils::document().element_dyn_into::<web_sys::HtmlElement>("help-msg")?
            .set_inner_text(Self::DEF_HELP_MSG);
        Ok(())
    }
}

impl yew::Component for Main {
    type Message = MainCmd;
    type Properties = ();

    fn create(ctx: &yew::Context<Self>) -> Self {
        *unsafe{&mut MAINCMD_SENDER} = Some(ctx.link().callback(|msg| msg));

        utils::js_try!{
            let player = web_sys::AudioContext::new()?;
            let analyser = Rc::new(web_sys::AnalyserNode::new(&player)?);
            let sound_comps = vec![
                sound::SoundFunctor::new_wave(&player, 0, utils::Point{x:300, y:350})?,
                sound::SoundFunctor::new_envelope(&player, 1, utils::Point{x:500, y:350})?,
                sound::SoundFunctor::new_builtin_output(analyser.clone(), 2, utils::Point{x:750, y:350})?];
            analyser.connect_with_audio_node(&player.destination())?;

            Self {analyser, sound_comps, player, error_count: 0,
                release_times: vec![(1, 0.0)],
                selected_comp: None, focused_comp: None, hovered_comp: None,
                plane_offset: utils::Point::ZERO, plane_moving: false,
                graph_canvas: wasm_bindgen::JsValue::UNDEFINED.unchecked_into(),
                sound_visualiser_canvas: wasm_bindgen::JsValue::UNDEFINED.unchecked_into(),
                editor_plane_canvas: wasm_bindgen::JsValue::UNDEFINED.unchecked_into()}
        }.expect_throw("initialising the main component")
    }

    fn update(&mut self, ctx: &yew::Context<Self>, msg: Self::Message) -> bool {
        let on_new_error = |this: &mut Self, err: wasm_bindgen::JsValue| -> bool {
            this.error_count += 1;
            web_sys::console::warn_1(&err);
            true
        };
        let err = utils::js_try!{type = !:
            let cur_time = self.player.current_time();
            let mut editor_plane_ctx: Option<(f64, f64, web_sys::CanvasRenderingContext2d)> = None;
            match msg {
                MainCmd::Drag(e) => utils::js_try!{
                    if let Some(id) = self.focused_comp {
                        editor_plane_ctx = Some((self.editor_plane_canvas.width().into(),
                            self.editor_plane_canvas.height().into(),
                            self.editor_plane_canvas.get_2d_context()?));
                        let comp = self.sound_comps.get_mut_or_js_error(id, "sound element #", " not found")?;
                        comp.handle_movement(Some(utils::Point{x: e.x(), y: e.y()} + self.plane_offset), ctx.link())?;
                    } else if self.plane_moving {
                        editor_plane_ctx = Some((self.editor_plane_canvas.width().into(),
                            self.editor_plane_canvas.height().into(),
                            self.editor_plane_canvas.get_2d_context()?));
                        self.plane_offset -= utils::Point{x: e.movement_x(), y: e.movement_y()};
                    } else {
                        if let Some(id) = self.hovered_comp {
                            let point = utils::Point{x: e.x(), y: e.y()} + self.plane_offset;
                            if !self.sound_comps.get_or_js_error(id, "sound element #", " not found")?.contains(point) {
                                self.hovered_comp = None;
                                Self::remove_desc()?;
                            }
                        } if self.hovered_comp.is_none() {
                            let point = utils::Point{x: e.x(), y: e.y()} + self.plane_offset;
                            if let Some(comp) = self.sound_comps.iter().find(|x| x.contains(point)) {
                                self.hovered_comp = Some(comp.id());
                                Self::set_desc(comp.name())?;
                            }
                        }
                    }
                }.explain_err("handling `MainCmd::Drag` message")?,

                MainCmd::Focus(e) => utils::js_try!{
                    self.editor_plane_canvas.set_pointer_capture(e.pointer_id())?;
                    editor_plane_ctx = Some((self.editor_plane_canvas.width().into(),
                        self.editor_plane_canvas.height().into(),
                        self.editor_plane_canvas.get_2d_context()?));
                    let point = utils::Point{x: e.x(), y: e.y()} + self.plane_offset;
                    self.focused_comp = self.sound_comps.iter()
                        .position(|c| c.contains(point));
                    if let Some(comp) = self.focused_comp {
                        let comp = unsafe {self.sound_comps.get_unchecked_mut(comp)};
                        comp.handle_movement(Some(utils::Point{x: e.x(), y: e.y()} + self.plane_offset), ctx.link())?;
                    } else {
                        self.plane_moving = true;
                        Self::set_desc("Dragging the plane")?;
                    }
                }.explain_err("handling `MainCmd::Focus` message")?,

                MainCmd::Unfocus(e) => utils::js_try!{
                    self.editor_plane_canvas.release_pointer_capture(e.pointer_id())?;
                    editor_plane_ctx = Some((self.editor_plane_canvas.width().into(),
                        self.editor_plane_canvas.height().into(),
                        self.editor_plane_canvas.get_2d_context()?));
                    if let Some(id) = self.focused_comp.take() {
                        let comp = self.sound_comps.get_mut_or_js_error(id, "sound functor #", " not found")?;
                        let receiver = ctx.link();
                        comp.handle_movement(Some(utils::Point{x:e.x(), y:e.y()} + self.plane_offset), receiver)?;
                        comp.handle_movement(None, receiver)?;
                        Self::set_desc(comp.name())?;
                    } else {
                       self.plane_moving = false;
                       Self::remove_desc()?;
                    }
                }.explain_err("handling `MainCmd::Unfocus` message")?,

                MainCmd::LeavePlane => utils::js_try!{
                    self.hovered_comp = None;
                    Self::remove_desc()?;
                }.explain_err("handling `MainCmd::LeavePlane` message")?,

                MainCmd::SetDesc(value) => Self::set_desc(&value)
                    .explain_err("handling `MainCmd::SetDesc` message")?,

                MainCmd::RemoveDesc => Self::remove_desc()
                    .explain_err("handling `MainCmd::RemoveDesc` message")?,

                MainCmd::SetParam(comp_id, param_id, value) => utils::js_try!{
                    let (graph, graph_in_span, graph_span) = self.sound_comps
                        .get_mut_or_js_error(comp_id, "sound component #", " not found")?
                        .set_param(param_id, value, cur_time, ctx.link())?
                        .graph(self.graph_canvas.width().into(), self.graph_canvas.height().into())?;
                    let mut ctx = ANIMATION_CTX.get_mut()?;
                    ctx.graph = graph;
                    ctx.graph_in_span = graph_in_span;
                    ctx.graph_span = graph_span;
                }.explain_err("handling `MainCmd::SetParam` message")?,

                MainCmd::SetGlobalReleaseTime(comp_id, val) =>
                    _ = self.release_times.iter_mut()
                        .find(|(id, _time)| *id == comp_id)
                        .map( |(_id, time)| *time = val).is_none().then(||
                            self.release_times.push((comp_id, val))),

                MainCmd::TryConnect(src_id, dst_pos) => utils::js_try!{
                    if let Some(dst_id) = self.sound_comps.iter().position(|x| x.contains(dst_pos)) {
                        if let Ok([src, dst]) = self.sound_comps.get_many_mut([src_id, dst_id]) {
                            src.connect(dst)?;
                        }
                    } else {
                        Self::remove_desc()?}
                    editor_plane_ctx = Some((self.editor_plane_canvas.width().into(),
                        self.editor_plane_canvas.height().into(),
                        self.editor_plane_canvas.get_2d_context()?));
                }.explain_err("handling `MainCmd::TryConnect` message")?,

                MainCmd::Select(id) => utils::js_try!{type = !:
                    self.selected_comp = (Some(id) != self.selected_comp).then_some(id);
                    if let Some(id) = self.selected_comp {
                        let (graph, graph_in_span, graph_span) = self.sound_comps
                            .get_or_js_error(id, "sound functor #", " not found")?
                            .graph(self.graph_canvas.width().into(), self.graph_canvas.height().into())?;
                        let mut ctx = ANIMATION_CTX.get_mut()?;
                        ctx.graph = graph;
                        ctx.graph_in_span = graph_in_span;
                        ctx.graph_span = graph_span;
                    } else {
                        ANIMATION_CTX.get_mut()?.graph_span = f64::NAN}
                    return true
                }.explain_err("handling `MainCmd::Select` message")?,

                MainCmd::Start => utils::js_try!{
                    ANIMATION_CTX.get_mut()?
                        .pbar_start = f64::INFINITY;

                    self.sound_comps.iter_mut()
                        .try_for_each(|comp| comp.start(cur_time))?
                }.explain_err("handling `MainCmd::Start` message")?,

                MainCmd::End => utils::js_try!{
                    ANIMATION_CTX.get_mut()?
                        .pbar_start = f64::NEG_INFINITY;

                    self.release_times.sort_unstable_by(|(_id1, t1), (_id2, t2)|
                        t1.partial_cmp(t2).unwrap_or(Ordering::Equal));
                    self.sound_comps.iter_mut()
                        .try_for_each(|comp| comp.end(cur_time, self.release_times.last().copied().unwrap_or((0, 0.0)).1))?
                }.explain_err("handling `MainCmd::End` message")?,

                MainCmd::ReportError(err) => return on_new_error(self, err)
            };

            if let Some((w, h, ctx)) = editor_plane_ctx {
                ctx.fill_rect(0.0, 0.0, w.into(), h.into());
                ctx.begin_path();
                self.sound_comps.iter()
                    .try_for_each(|c| c.draw(&ctx, self.plane_offset, &self.sound_comps))
                    .explain_err("redrawing the editor plane")?;
            }
            return false
        };
        on_new_error(self, err.into_err())
    }

    fn view(&self, ctx: &yew::Context<Self>) -> yew::Html {
        let comp = self.selected_comp.and_then(|i| self.sound_comps.get(i));

        return yew::html! {<>
            <canvas width="100%" height="100%" id="plane"
            onpointerdown={ctx.link().callback(MainCmd::Focus)}
            onpointerup={ctx.link().callback(MainCmd::Unfocus)}
            onpointermove={ctx.link().callback(MainCmd::Drag)}
            onpointerleave={ctx.link().callback(|_| MainCmd::LeavePlane)}/>
            <div id="ctrl-panel">
                <div id="help-msg" style="user-select:none">{Self::DEF_HELP_MSG}</div>
                <div id="inputs">
                    {comp.map(sound::SoundFunctor::params)}
                </div>
                <canvas id="graph" class="visual"
                hidden={!comp.is_some_and(|comp| comp.graphable())}/>
            </div>
            <div id="visuals">
                <canvas id="sound-visualiser" class="visual"/>
                <button
                onmousedown={ctx.link().callback(|_| MainCmd::Start)}
                onmouseup={ctx.link().callback(|_| MainCmd::End)}>
                    {"Play"}
                </button>
            </div>
            if self.error_count > 0 {
                <div id="error-count">{format!("Errors: {}", self.error_count)}</div>
            }
        </>}
    }

    fn rendered(&mut self, _: &yew::Context<Self>, first_render: bool) {
        _ = utils::js_try!{type = !:
            return if first_render {
                self.graph_canvas = utils::document().element_dyn_into("graph")?;
                self.graph_canvas.set_hidden(false);
                self.graph_canvas.sync();
                self.graph_canvas.set_hidden(true);
                self.sound_visualiser_canvas = utils::document().element_dyn_into("sound-visualiser")?;
                self.sound_visualiser_canvas.sync();
                let (width, height) = utils::js_try!{
                    self.editor_plane_canvas = utils::document().element_dyn_into("plane")?;
                    let body = utils::document().body().to_js_result("<body> not found")?;
                    let [width, height] = [body.client_width(), body.client_height()];
                    self.editor_plane_canvas.set_width(width as u32);
                    self.editor_plane_canvas.set_height(height as u32);
                    (width, height)
                }.explain_err("initialising the editor plane")?;
                utils::js_try!{
                    let ctx = self.editor_plane_canvas.get_2d_context()?;
                    ctx.set_stroke_style(&"#0069E1".into());
                    ctx.set_fill_style(&"#232328".into());
                    ctx.set_line_width(2.0);
                    ctx.fill_rect(0.0, 0.0, width.into(), height.into());
                    ctx.begin_path();
                    self.sound_comps.iter()
                        .try_for_each(|c| c.draw(&ctx, utils::Point::ZERO, &self.sound_comps))?;
                }.explain_err("drawing the editor plane for the first time")?;

                ANIMATION_CTX.set(AnimationCtx {
                    analyser: self.analyser.clone(), renderer: render::Renderer::new(),
                    sound_visualiser_canvas: self.sound_visualiser_canvas.clone(),
                    graph_canvas: self.graph_canvas.clone(),
                    solid_line: js_sys::Array::new().into(),
                    dotted_line: js_sys::Array::of2(&(10.0).into(), &(10.0).into()).into(),
                    graph: web_sys::Path2d::new()?,
                    graph_in_span: f64::NAN,
                    graph_span: f64::NAN,
                    pbar_start: f64::NAN,
                    js_callback: Default::default() // initialized later in `start_animation_loop`
                })?;
                start_animation_loop()?;
            }
        }.expect_throw("rendering the main element");
    }
}

fn main() {
    yew::Renderer::<Main>::new().render();
}
