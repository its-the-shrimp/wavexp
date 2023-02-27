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
use utils::{JsResultUtils, OkOrJsError, HtmlCanvasExt, HtmlDocumentExt, SliceExt};
use web_sys;
use js_sys;
use wasm_bindgen;
use wasm_bindgen::JsCast;
use yew::TargetCast;
use std::rc::Rc;

struct AnimationCtx {
    analyser: Rc<web_sys::AnalyserNode>,
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
        _ = utils::js_try!{
                let mut handle = ANIMATION_CTX.get_mut()?;
                let AnimationCtx{ref analyser, ref mut renderer, 
                    ref graph, ref solid_line, ref dotted_line,
                    ref graph_in_span, ref graph_span, ref mut pbar_start,
                    ref js_callback} = *handle;
                let graph_out_span = graph_span - graph_in_span;
                let (w, h, ctx) = utils::get_canvas_ctx("main", false, false)?;
                let buf = renderer.set_size(w as usize, h as usize).get_in_buffer();
                analyser.get_byte_frequency_data(buf);
                ctx.put_image_data(
                    &web_sys::ImageData::new_with_u8_clamped_array(
                        wasm_bindgen::Clamped(renderer.graph().get_out_bytes()), w as u32)?,
                    0.0, 0.0)?;

                if let Ok((w, h, ctx)) = utils::get_canvas_ctx("graph", true, true) {
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
                }
                
                utils::window().request_animation_frame(js_callback)?;
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

struct Main {
    player: web_sys::AudioContext,
    help_msg: Rc<str>,
    selected_comp: Option<usize>,
    focused_comp: Option<usize>,
    hovered_comp: Option<usize>,
    error_count: usize,
    plane_moving: bool,
    plane_offset: utils::Point,
    sound_comps: Vec<sound::SoundFunctor>
}

#[derive(Debug)]
pub enum MainCmd {
    Drag(web_sys::PointerEvent),
    Focus(web_sys::PointerEvent),
    Unfocus(web_sys::PointerEvent),
    LeavePlane,
    SetDesc(Rc<str>),
    RemoveDesc,
    SetParam(usize, usize, f64),
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

            ANIMATION_CTX.set(AnimationCtx {
                analyser,
                renderer: render::Renderer::new(),
                solid_line: js_sys::Array::new().into(),
                dotted_line: js_sys::Array::of2(&(10.0).into(), &(10.0).into()).into(),
                graph: web_sys::Path2d::new()?,
                graph_in_span: f64::NAN,
                graph_span: f64::NAN,
                pbar_start: f64::NAN,
                js_callback: Default::default() // initialized later in `start_animation_loop`
            })?;

            Self {help_msg: Self::DEF_HELP_MSG.into(),
                sound_comps, player,
                error_count: 0, plane_moving: false,
                selected_comp: None, focused_comp: None, hovered_comp: None,
                plane_offset: utils::Point::ZERO}
        }.expect_throw("initialising the main component")
    }

    fn update(&mut self, _: &yew::Context<Self>, msg: Self::Message) -> bool {
        let on_new_error = |this: &mut Self, err: wasm_bindgen::JsValue| -> bool {
            this.error_count += 1;
            web_sys::console::warn_1(&err);
            true
        };
        let err = utils::js_try!{type = !:
            let cur_time = self.player.current_time();
            let mut editor_plane_ctx: Option<(f64, f64, web_sys::CanvasRenderingContext2d)> = None;
            let mut needs_html_rerender = false;
            match msg {
                MainCmd::Drag(e) => utils::js_try!{
                    if let Some(id) = self.focused_comp {
                        let target = e.target_dyn_into::<web_sys::HtmlCanvasElement>()
                            .ok_or_js_error("the event has no target")?;
                        editor_plane_ctx = Some((target.width().into(),
                            target.height().into(),
                            target.get_2d_context()?));
                        let comp = self.sound_comps.get_mut_or_js_error(id, "sound element #", " not found")?;
                        comp.handle_movement(Some(utils::Point{x: e.x(), y: e.y()} + self.plane_offset))?;
                    } else if self.plane_moving {
                        let target = e.target_dyn_into::<web_sys::HtmlCanvasElement>()
                            .ok_or_js_error("the event has no target")?;
                        let [w, h] = [target.width() as i32, target.height() as i32];
                        editor_plane_ctx = Some((w as f64, h as f64, target.get_2d_context()?));
                        self.plane_offset -= utils::Point{x: e.movement_x(), y: e.movement_y()};
                    } else if let Some(id) = self.hovered_comp {
                        let point = utils::Point{x: e.x(), y: e.y()} + self.plane_offset;
                        if !self.sound_comps.get_or_js_error(id, "sound element #", " not found")?.contains(point) {
                            self.hovered_comp = None;
                            MainCmd::RemoveDesc.send();
                        }
                    } if self.hovered_comp.is_none() {
                        let point = utils::Point{x: e.x(), y: e.y()} + self.plane_offset;
                        if let Some(comp) = self.sound_comps.iter().find(|x| x.contains(point)) {
                            self.hovered_comp = Some(comp.id());
                            MainCmd::SetDesc(comp.name().into()).send();
                        }
                    }
                }.explain_err("handling `MainCmd::Drag` message")?,

                MainCmd::Focus(e) => utils::js_try!{
                    let target = e.target_dyn_into::<web_sys::HtmlCanvasElement>()
                        .ok_or_js_error("the event has no target")?;
                    editor_plane_ctx = Some((target.width().into(), target.height().into(), target.get_2d_context()?));
                    let point = utils::Point{x: e.x(), y: e.y()} + self.plane_offset;
                    self.focused_comp = self.sound_comps.iter()
                        .find(|c| c.contains(point)).map(|c| c.id());
                    self.plane_moving = self.focused_comp.is_none();
                }.explain_err("handling `MainCmd::Focus` message")?,

                MainCmd::Unfocus(e) => utils::js_try!{
                    let target = e.target_dyn_into::<web_sys::HtmlCanvasElement>()
                        .ok_or_js_error("the event has no target")?;
                    editor_plane_ctx = Some((target.width().into(), target.height().into(), target.get_2d_context()?));
                    self.plane_moving = false;
                    if let Some(id) = self.focused_comp.take() {
                        let comp = self.sound_comps.get_mut_or_js_error(id, "sound functor #", " not found")?;
                        comp.handle_movement(Some(utils::Point{x:e.x(), y:e.y()} + self.plane_offset))?;
                        comp.handle_movement(None)?;
                    }
                }.explain_err("handling `MainCmd::Unfocus` message")?,

                MainCmd::LeavePlane => self.hovered_comp = None,

                MainCmd::SetDesc(value) => {
                    self.help_msg = format!("{:1$}", value, Self::DEF_HELP_MSG.len()).into();
                    needs_html_rerender = true}

                MainCmd::RemoveDesc => {
                    self.help_msg = Self::DEF_HELP_MSG.into();
                    needs_html_rerender = true}

                MainCmd::SetParam(comp_id, param_id, value) => utils::js_try!{
                    let comp = self.sound_comps.get_mut_or_js_error(comp_id, "sound functor #", " not found")?;
                    comp.set_param(param_id, value, cur_time)?;
                    let (w, h, _) = utils::get_canvas_ctx("graph", false, false)?;
                    let (graph, graph_in_span, graph_span) = comp.graph(w, h)?;
                    let mut ctx = ANIMATION_CTX.get_mut()?;
                    ctx.graph = graph;
                    ctx.graph_in_span = graph_in_span;
                    ctx.graph_span = graph_span;
                }.explain_err("handling `MainCmd::SetParam` message")?,

                MainCmd::TryConnect(src_id, dst_pos) => utils::js_try!{
                    if let Some(dst_id) = self.sound_comps.iter().position(|x| x.contains(dst_pos)) {
                        if let Ok([src, dst]) = self.sound_comps.get_many_mut([src_id, dst_id]) {
                            src.connect(dst)?;
                        }
                    } else {
                        self.help_msg = Self::DEF_HELP_MSG.into();
                    }
                    let target = utils::document().element_dyn_into::<web_sys::HtmlCanvasElement>("plane")?;
                    editor_plane_ctx = Some((target.width().into(),
                        target.height().into(),
                        target.get_2d_context()?));
                }.explain_err("handling `MainCmd::TryConnect` message")?,

                MainCmd::Select(id) => utils::js_try!{
                    self.selected_comp = (Some(id) != self.selected_comp).then_some(id);
                    if let Some(id) = self.selected_comp {
                        let (w, h, _) = utils::get_canvas_ctx("graph", false, false)?;
                        let (graph, graph_in_span, graph_span) = self.sound_comps
                            .get_or_js_error(id, "sound functor #", " not found")?
                            .graph(w, h)?;
                        let mut ctx = ANIMATION_CTX.get_mut()?;
                        ctx.graph = graph;
                        ctx.graph_in_span = graph_in_span;
                        ctx.graph_span = graph_span;
                    }
                    needs_html_rerender = true
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

                    self.sound_comps.iter_mut()
                        .try_for_each(|comp| comp.end(cur_time))?
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
            return needs_html_rerender
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
                <div id="help-msg">{self.help_msg.clone()}</div>
                <div id="inputs">
                    {comp.map(sound::SoundFunctor::params)}
                </div>
                <canvas id="graph" class="visual"
                hidden={!comp.is_some_and(|comp| comp.graphable())}/>
            </div>
            <div id="visuals">
                <canvas id="main" class="visual"/>
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
                start_animation_loop()?;
                let (width, height, plane) = utils::js_try!{
                    utils::sync_canvas("main")?;
                    let graph: web_sys::HtmlElement = utils::document().element_dyn_into("graph")?;
                    graph.set_hidden(false);
                    utils::sync_canvas("graph")?;
                    graph.set_hidden(true);
                    let body = utils::document().body().ok_or_js_error("<body> not found")?;
                    let [width, height] = [body.client_width(), body.client_height()];
                    let plane: web_sys::HtmlCanvasElement = utils::document().element_dyn_into("plane")?;
                    plane.set_width(width as u32);
                    plane.set_height(height as u32);
                    (width, height, plane)
                }.explain_err("initialising the editor plane")?;
                utils::js_try!{
                    let plane = plane.get_2d_context()?;
                    plane.set_stroke_style(&"#0069E1".into());
                    plane.set_fill_style(&"#232328".into());
                    plane.set_line_width(2.0);
                    plane.fill_rect(0.0, 0.0, width.into(), height.into());
                    plane.begin_path();
                    self.sound_comps.iter()
                        .try_for_each(|c| c.draw(&plane, utils::Point::ZERO, &self.sound_comps))?;
                }.explain_err("drawing the editor plane for the first time")?;
            }
        }.report_err("rendering the main element");
    }
}

fn main() {
    yew::Renderer::<Main>::new().render();
}
