#![feature(get_many_mut)]
#![feature(is_some_and)]
mod render;
mod utils;
mod input;
mod sound;
use utils::{ResultUtils, JsResultUtils, Pipe};
use web_sys;
use js_sys;
use wasm_bindgen;
use wasm_bindgen::JsCast;
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

static mut ANIMATION_CTX: Option<std::sync::Mutex<AnimationCtx>> = None;

fn start_animation_loop() {
    fn render(time: f64) {
        if let Ok(AnimationCtx {analyser, renderer,
            graph, solid_line, dotted_line, 
            graph_in_span, graph_span, pbar_start,
            js_callback }) = unsafe{ANIMATION_CTX.as_ref().unwrap_unchecked()}.lock().as_deref_mut() {
            let graph_out_span = *graph_span - *graph_in_span;

            if let Some((w, h, ctx)) = utils::get_canvas_ctx("main", false, false) {
                let buf = renderer.set_size(w as usize, h as usize).get_in_buffer();
                analyser.get_byte_frequency_data(buf);
                ctx.put_image_data(
                    &web_sys::ImageData::new_with_u8_clamped_array(
                        wasm_bindgen::Clamped(renderer.graph().get_out_bytes()), w as u32)
                        .expect_throw_val("preparing the rendered audio visualisation"),
                    0.0, 0.0)
                    .expect_throw_val("outputting the rendered audio visualisation");
            }

            if let Some((w, h, ctx)) = utils::get_canvas_ctx("graph", true, true) {
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
                            ctx.set_line_dash(dotted_line)
                                .expect_throw_val("drawing the envelope progress bar");
                            ctx.set_line_width(1.0);
                            ctx.set_line_dash_offset(time / 100.0);
                            ctx.begin_path();
                            ctx.move_to(x, 0.0);
                            ctx.line_to(x, h);
                            ctx.stroke();
                            ctx.set_line_dash(solid_line)
                                .expect_throw_val("drawing the graph");
                        }
                    }
                }
            }
            
            utils::window()
                .request_animation_frame(js_callback)
                .expect_throw_val("rescheduling the animation callback");
        }
    }

    for name in ["main", "graph"] {
        if let Some((_, _, ctx)) = utils::get_canvas_ctx(name, false, false) {
            ctx.set_stroke_style(&"#0069E1".into());
            ctx.set_fill_style(&"#181818".into());
        }
    }

    unsafe{ANIMATION_CTX
        .as_mut().unwrap_unchecked()
        .get_mut().unwrap_unchecked()}
        .js_callback = wasm_bindgen::closure::Closure::<dyn Fn(f64)>::new(render)
            .into_js_value().unchecked_into::<js_sys::Function>();
// this is safe because the code haven't yet branched
// (i.e. `request_animation_frame` wasn't yet called)
// and `ANIMATION_CTX` was already initialized
    render(0.0);
}

struct Main {
    player: web_sys::AudioContext,
    help_msg: Rc<str>,
    selected_comp: Option<usize>
}

pub fn sound_comps() -> &'static mut Vec<sound::SoundFunctor> {
    static mut RES: Vec<sound::SoundFunctor> = Vec::new();
    unsafe{&mut RES}
}

#[derive(Debug)]
pub enum MainCmd {
    SetDesc(Rc<str>),
    RemoveDesc(),
    Select(Option<usize>),
    TryConnect(usize, i32, i32),
    SetParam(usize, usize, f64),
    Start(), 
    End(),
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

        let player = web_sys::AudioContext::new()
            .expect_throw_val("creating the audio context");
        let analyser = Rc::new(web_sys::AnalyserNode::new(&player)
            .expect_throw_val("creating the main audio visualiser"));
        analyser.connect_with_audio_node(&player.destination())
            .expect_throw_val("connecting the analyser with the built-in output node");
        *sound_comps() = vec![
            sound::SoundFunctor::new_wave(&player, 0, 300, 350)
                .expect_throw_val("create the generator component"), 
            sound::SoundFunctor::new_envelope(&player, 1, 500, 350)
                .expect_throw_val("creating the envelope component"),
            sound::SoundFunctor::new_builtin_output(analyser.clone(), 2, 750, 350)
                .expect_throw_val("creating the output component")];

// initalizing context for the animation loop
        *unsafe{&mut ANIMATION_CTX} = Some(std::sync::Mutex::new(AnimationCtx {
            analyser,
            renderer: render::Renderer::new(),
            solid_line: js_sys::Array::new().into(),
            dotted_line: js_sys::Array::of2(&(10.0).into(), &(10.0).into()).into(),
            graph: web_sys::Path2d::new()
                .expect_throw_val("initializing an empty envelope graph"),
            graph_in_span: f64::NAN,
            graph_span: f64::NAN,
            pbar_start: f64::NAN,
            js_callback: Default::default() // initialized later in `start_animation_loop`
        }));

        Self {help_msg: Self::DEF_DESC.into(),
            selected_comp: None, player}
    }

    fn update(&mut self, _: &yew::Context<Self>, msg: Self::Message) -> bool {
        let cur_time = self.player.current_time();
        match msg {
            MainCmd::SetDesc(value) =>{
                self.help_msg = format!("{:1$}", value, Self::DEF_DESC.len()).into();
            return true}
            MainCmd::RemoveDesc() => {
                self.help_msg = Self::DEF_DESC.into();
            return true}
            MainCmd::Select(id) => {
                self.selected_comp = id.filter(|&x| Some(x) != self.selected_comp);
                if let Ok(mut ctx) = unsafe{ANIMATION_CTX.as_ref().unwrap_unchecked()}.lock() {
                    ctx.graph_span = f64::NAN;
                }
            return true}

            MainCmd::TryConnect(src_id, x, y) => 
                if let Some(dst_id) = sound_comps().iter().position(|dst| dst.contains(x, y)) {
                    if let Ok([src, dst]) = sound_comps().get_many_mut([src_id, dst_id]) {
                        src.connect(dst)
                            .expect_throw_val("executing `MainCmd::TryConnect` command");
                    }
                }

            MainCmd::SetParam(component_id, param_id, value) =>
                if let Some(comp) = sound_comps().get_mut(component_id) {
                    comp.set_param(param_id, value, cur_time)
                        .expect_throw_val("setting the component's parameter");
                    if let Some((w, h, _)) = utils::get_canvas_ctx("graph", false, false) {
                        if let Ok(mut ctx) = unsafe{ANIMATION_CTX.as_ref().unwrap_unchecked()}.lock() {
                            if let Ok((graph, graph_in_span, graph_span)) = comp.graph(w, h) {
                                ctx.graph = graph;
                                ctx.graph_in_span = graph_in_span;
                                ctx.graph_span = graph_span;
                            } else {
                                ctx.graph_span = f64::NAN;
                            }
                        }
                    }
                }

            MainCmd::Start() => {
                if let Ok(mut ctx) = unsafe{ANIMATION_CTX.as_ref().unwrap_unchecked()}.lock() {
                    ctx.pbar_start = f64::INFINITY}

                sound_comps().iter_mut().try_for_each(|comp| comp.start(cur_time))
                    .expect_throw_val("starting a sound component")
            }

            MainCmd::End() => {
                if let Ok(mut ctx) = unsafe{ANIMATION_CTX.as_ref().unwrap_unchecked()}.lock() {
                    ctx.pbar_start = f64::NEG_INFINITY}

                sound_comps().iter_mut().try_for_each(|comp| comp.end(cur_time))
                    .expect_throw_val("ending a sound component")
            }
        }
        false
    }

    fn view(&self, ctx: &yew::Context<Self>) -> yew::Html {
        let comp = self.selected_comp.and_then(|i| sound_comps().get(i));
        yew::html! {<>
            <svg width="100%" height="100%"
            viewBox={utils::document().body()
                .expect_throw("failed because the whole freaking body is absent for some reason").pipe(|body|
                   format!("0 0 {} {}", body.client_width(), body.client_height()))}>
                {for sound_comps().iter().map(sound::SoundFunctor::as_html)}
            </svg>
            <div id="ctrl-panel">
                <div id="help-msg">{self.help_msg.clone()}</div>
                <div id="inputs">
                    {comp.map(sound::SoundFunctor::params)}
                </div>
                if comp.map(|comp| comp.graphable()).unwrap_or(false) {
                    <canvas id="graph" class="visual"/>
                }
            </div>
            <div id="visuals">
                <canvas id="main" class="visual"/>
                <button
                onmousedown={ctx.link().callback(|_| MainCmd::Start())}
                onmouseup={ctx.link().callback(|_| MainCmd::End())}>
                    {"Play"}
                </button>
            </div>
        </>}
    }

    fn rendered(&mut self, _: &yew::Context<Self>, first_render: bool) {
        if first_render {
            start_animation_loop();
            utils::sync_canvas("main");
        }
        utils::sync_canvas("graph");
    }
}

fn main() {
    yew::Renderer::<Main>::new().render();
}
