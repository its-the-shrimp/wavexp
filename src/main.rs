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
use utils::{JsResultUtils, OkOrJsError, ToJsResult, Tee};
use web_sys;
use js_sys;
use wasm_bindgen;
use wasm_bindgen::JsCast;
use std::rc::Rc;
use std::cell::{Cell, RefCell};

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
        let err: utils::JsResult<!> = try {
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
        };
        _ = err.report_err("rendering frame-by-frame animation");
    }

    ANIMATION_CTX.get_mut()
        .explain_err("starting the animation loop")?
        .js_callback = wasm_bindgen::closure::Closure::<dyn Fn(f64)>::new(render)
            .into_js_value().unchecked_into::<js_sys::Function>();
    render(0.0);
    Ok(())
}

pub static SOUND_COMPS: utils::WasmCell<RefCell<Vec<sound::SoundFunctor>>> = utils::WasmCell::new(RefCell::new(Vec::new()));
pub static PLANE_OFFSET: utils::WasmCell<Cell<[i32; 2]>> = utils::WasmCell::new(Cell::new([0, 0]));

pub fn get_sound_comp(id: usize) -> utils::JsResult<std::cell::Ref<'static, sound::SoundFunctor>> {
    std::cell::Ref::filter_map(SOUND_COMPS.try_borrow().to_js_result()?, |x| x.get(id)).ok()
        .ok_or_js_error_with(|| format!("sound functor #{} not found", id))
}

pub fn get_sound_comp_mut(id: usize) -> utils::JsResult<std::cell::RefMut<'static, sound::SoundFunctor>> {
    std::cell::RefMut::filter_map(SOUND_COMPS.try_borrow_mut().to_js_result()?, |x| x.get_mut(id)).ok()
        .ok_or_js_error_with(|| format!("sound functor #{} not found", id))
}

struct Main {
    player: web_sys::AudioContext,
    help_msg: Rc<str>,
    selected_comp: Option<usize>,
    focused_comp: Option<usize>,
    error_count: usize,
    initial: Option<[i32; 2]>
}

#[derive(Debug)]
pub enum MainCmd {
    ModifyPlane(draggable::Cmd),
    SetDesc(Rc<str>),
    RemoveDesc(),
    Select(Option<usize>),
    SetParam(usize, usize, f64),
    Start(), 
    End(),
    ReportError(wasm_bindgen::JsValue)
}

static mut MAINCMD_SENDER: Option<yew::Callback<MainCmd>> = None;
impl MainCmd {
    #[inline] pub fn send(self) {
        unsafe{MAINCMD_SENDER.as_ref().unwrap_unchecked()}.emit(self)
    }
}

impl Main {
    const DEF_DESC: &'static str = "Hover over an element to get help";
}

impl yew::Component for Main {
    type Message = MainCmd;
    type Properties = ();

    fn create(ctx: &yew::Context<Self>) -> Self {
        *unsafe{&mut MAINCMD_SENDER} = Some(ctx.link().callback(|msg| msg));

        let err: utils::JsResult<!> = try {
            let player = web_sys::AudioContext::new()?;
            let analyser = Rc::new(web_sys::AnalyserNode::new(&player)?);
            analyser.connect_with_audio_node(&player.destination())?;
            *SOUND_COMPS.try_borrow_mut().to_js_result()? = vec![
                sound::SoundFunctor::new_wave(&player, 0, 300, 350)?,
                sound::SoundFunctor::new_envelope(&player, 1, 500, 350)?,
                sound::SoundFunctor::new_builtin_output(analyser.clone(), 2, 750, 350)?];

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

            return Self {help_msg: Self::DEF_DESC.into(),
                player, error_count: 0, initial: None,
                selected_comp: None, focused_comp: None}
        };
        err.expect_throw("initialising the main component");
    }

    fn update(&mut self, _: &yew::Context<Self>, msg: Self::Message) -> bool {
        let on_new_error = |this: &mut Self, err: wasm_bindgen::JsValue| -> bool {
            this.error_count += 1;
            web_sys::console::warn_1(&err);
            true
        };
        let err: utils::JsResult<!> = try {
            let cur_time = self.player.current_time();
            match msg {
                MainCmd::ModifyPlane(cmd) => {
                    let (w, h, ctx) = utils::get_canvas_ctx("plane", true, false)?;
                    let [w, h] = [w as i32, h as i32];
                    let mut offset = PLANE_OFFSET.get();

                    let mut needs_redraw: bool = true;
                    cmd.handle_drag(|e| Ok({
                            if let Some(id) = self.focused_comp {
                                let mut comp = get_sound_comp_mut(id)?;
                                comp.handle_movement(Some([e.x(), e.y()]))?;
                            } else if let Some(initial) = self.initial {
                                PLANE_OFFSET.set([
                                    (initial[0] - e.x()).clamp(-w, w),
                                    (initial[1] - e.y()).clamp(-h, h)])
                            } else {needs_redraw = false}
                        }))?
                        .handle_focus(|e| Ok({
                            let [x, y] = [e.x() + offset[0], e.y() + offset[1]];
                            self.focused_comp = SOUND_COMPS.try_borrow().to_js_result()?
                                .iter().find(|c| c.contains(x, y)).map(|c| c.id());
                            if self.focused_comp.is_none() {
                                self.initial = Some([x, y])}
                        }))?
                        .handle_unfocus(|e| Ok({
                            let [x, y] = [e.x() + offset[0], e.y() + offset[1]];
                            self.initial = None;
                            if let Some(src_id) = self.focused_comp.take() {
                                let mut comps = SOUND_COMPS.try_borrow_mut().to_js_result()?;
                                if let Some(dst_id) = comps.iter().position(|dst| dst.contains(x, y)) {
                                    MainCmd::Select(Some(dst_id)).send();
                                    if let Ok([src, dst]) = comps.get_many_mut([src_id, dst_id]) {
                                        src.connect(dst)?;
                                        src.handle_movement(None)?;
                                    }
                                } else {
                                    self.help_msg = Self::DEF_DESC.into();
                                }
                                comps.get_mut(src_id)
                                    .ok_or_js_error_with(||
                                        format!("while processing mouse focus release: sound functor #{} not found", src_id))?
                                    .handle_movement(None)?;
                            }
                        }))?;

                    if needs_redraw {
                        ctx.fill_rect(0.0, 0.0, w.into(), h.into());
                        ctx.begin_path();
                        offset = PLANE_OFFSET.get();
                        SOUND_COMPS.try_borrow().to_js_result()?
                            .iter().try_for_each(|c| c.draw(&ctx, offset))?;
                    }
                }

                MainCmd::SetDesc(value) => {
                    self.help_msg = format!("{:1$}", value, Self::DEF_DESC.len()).into();
                    return true}

                MainCmd::RemoveDesc() => {
                    self.help_msg = Self::DEF_DESC.into();
                    return true}

                MainCmd::Select(id) => {
                    self.selected_comp = id.filter(|&x| Some(x) != self.selected_comp);
                    if let Some(id) = self.selected_comp {
                        let (w, h, _) = utils::get_canvas_ctx("graph", false, false)?;
                        let (graph, graph_in_span, graph_span) = get_sound_comp(id)?
                            .graph(w, h)?;
                        let mut ctx = ANIMATION_CTX.get_mut()?;
                        ctx.graph = graph;
                        ctx.graph_in_span = graph_in_span;
                        ctx.graph_span = graph_span;
                    }
                    return true}

                MainCmd::SetParam(component_id, param_id, value) => {
                    let mut comp = get_sound_comp_mut(component_id)?;
                    comp.set_param(param_id, value, cur_time)?;
                    let (w, h, _) = utils::get_canvas_ctx("graph", false, false)?;
                    let (graph, graph_in_span, graph_span) = comp.graph(w, h)?;
                    let mut ctx = ANIMATION_CTX.get_mut()?;
                    ctx.graph = graph;
                    ctx.graph_in_span = graph_in_span;
                    ctx.graph_span = graph_span;
                }

                MainCmd::Start() => {
                    ANIMATION_CTX.get_mut()?
                        .pbar_start = f64::INFINITY;

                    SOUND_COMPS.try_borrow_mut().to_js_result()?.iter_mut()
                        .try_for_each(|comp| comp.start(cur_time))?
                }

                MainCmd::End() => {
                    ANIMATION_CTX.get_mut()?
                        .pbar_start = f64::NEG_INFINITY;

                    SOUND_COMPS.try_borrow_mut().to_js_result()?.iter_mut()
                        .try_for_each(|comp| comp.end(cur_time))?
                }

                MainCmd::ReportError(err) => return on_new_error(self, err)
            };
            return false
        };
        on_new_error(self, err.explain_err("handling a message to the main component").into_err())
    }

    fn view(&self, ctx: &yew::Context<Self>) -> yew::Html {
        let err: utils::JsResult<!> = try {
            let comps = SOUND_COMPS.try_borrow().to_js_result()?;
            let comp = self.selected_comp.and_then(|i| comps.get(i));

            return yew::html! {<>
                <canvas width="100%" height="100%" id="plane"
                onpointerdown={ctx.link().callback(|e| MainCmd::ModifyPlane(draggable::Cmd::Focus(e)))}
                onpointerup={ctx.link().callback(|e| MainCmd::ModifyPlane(draggable::Cmd::Unfocus(e)))}
                onpointermove={ctx.link().callback(|e| MainCmd::ModifyPlane(draggable::Cmd::Drag(e)))}/>
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
                    onmousedown={ctx.link().callback(|_| MainCmd::Start())}
                    onmouseup={ctx.link().callback(|_| MainCmd::End())}>
                        {"Play"}
                    </button>
                </div>
                if self.error_count > 0 {
                    <div id="error-count">{format!("Errors: {}", self.error_count)}</div>
                }
            </>}
        };
        _ = err.report_err("building the main element");
        Default::default()
    }

    fn rendered(&mut self, _: &yew::Context<Self>, first_render: bool) {
        let err: utils::JsResult<!> = try {
            return if first_render {
                start_animation_loop()?;
                utils::sync_canvas("main")?;
                let graph = utils::document().get_element_by_id("graph")
                    .ok_or_js_error("element #graph not found")?
                    .unchecked_into::<web_sys::HtmlElement>();
                graph.set_hidden(false);
                utils::sync_canvas("graph")?;
                graph.set_hidden(true);
                let body = utils::document().body().ok_or_js_error("<body> not found")?;
                let [width, height] = [body.client_width(), body.client_height()];
                let plane = utils::document().get_element_by_id("plane")
                    .ok_or_js_error("element #plane not found")?
                    .unchecked_into::<web_sys::HtmlCanvasElement>();
                plane.set_width(width as u32);
                plane.set_height(height as u32);
                let plane = plane.get_context("2d")?
                    .ok_or_js_error("rendering context for #plane not found")?
                    .unchecked_into::<web_sys::CanvasRenderingContext2d>();
                plane.set_stroke_style(&"#0069E1".into());
                plane.set_fill_style(&"#232328".into());
                plane.set_line_width(2.0);
            }
        };
        _ = err.report_err("rendering the main element");
    }
}

fn main() {
    yew::Renderer::<Main>::new().render();
}
