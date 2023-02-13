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
    envelope_graph: web_sys::Path2d,
    envelope_in_time: f64,
    envelope_span: f64,
    envelope_pbar_start: f64,
    solid_line: wasm_bindgen::JsValue,
    dotted_line: wasm_bindgen::JsValue,
    renderer: render::Renderer,
    js_callback: js_sys::Function
}

static mut ANIMATION_CTX: Option<std::sync::Mutex<AnimationCtx>> = None;

fn start_animation_loop() {
    fn render(time: f64) {
        unsafe{ANIMATION_CTX.as_ref().unwrap_unchecked()}.lock().map(|mut handle| {
            let ctx = &mut *handle;
            let (main_width, main_height, main_ctx) = unsafe {
                utils::get_canvas_ctx("main",
                    &utils::js_obj!{ bool antialias: false, bool alpha: false })
                    .unwrap_unchecked()};
            let (envelope_width, envelope_height, envelope_ctx) = unsafe {
                utils::get_canvas_ctx("envelope", &utils::js_obj!{ bool alpha: false })
                    .unwrap_unchecked()};
    // `unwrap_unchecked` here is safe because the enclosing function is only called after
    // the canvases "main" and "envelope" has been shown to the user
            let buf = ctx.renderer.set_size(main_width as usize, main_height as usize).get_in_buffer();
            ctx.analyser.get_byte_frequency_data(buf);
            main_ctx.put_image_data(
                &web_sys::ImageData::new_with_u8_clamped_array(
                    wasm_bindgen::Clamped(ctx.renderer.graph().get_out_bytes()),
                    main_width as u32)
                    .expect_throw_val("preparing the rendered audio visualisation"),
                0.0, 0.0)
                .expect_throw_val("outputting the rendered audio visualisation");
            
            /*envelope_ctx.fill_rect(0.0, 0.0, envelope_width, envelope_height);
            envelope_ctx.set_line_dash(&ctx.solid_line)
                .expect_throw_val("drawing the envelope graph");
            envelope_ctx.set_line_width(3.0);
            envelope_ctx.stroke_with_path(&ctx.envelope_graph);
            if !ctx.envelope_pbar_start.is_nan() {
                envelope_ctx.set_line_dash(&ctx.dotted_line)
                    .expect_throw_val("drawing the envelope progress bar");
                envelope_ctx.set_line_width(1.0);
                envelope_ctx.set_line_dash_offset(time / 100.0);
                if ctx.envelope_pbar_start.is_infinite() {
                    ctx.envelope_pbar_start = time.copysign(ctx.envelope_pbar_start) / 1000.0
                }
                let x = if ctx.envelope_pbar_start.is_sign_positive() {
                    (time / 1000.0 - ctx.envelope_pbar_start).min(ctx.envelope_in_time)
                } else {
                    time / 1000.0 + ctx.envelope_pbar_start + ctx.envelope_in_time
                } / ctx.envelope_span * envelope_width;
                envelope_ctx.begin_path();
                envelope_ctx.move_to(x, 0.0);
                envelope_ctx.line_to(x, envelope_height);
                envelope_ctx.stroke()}*/

            utils::window()
                .request_animation_frame(&ctx.js_callback)
                .expect_throw_val("rescheduling the animation callback");
        }).expect_throw("getting the animation context from the animation loop")
    }

    let (_, _, main_ctx) = unsafe {
        utils::get_canvas_ctx("main",
            &utils::js_obj!{ bool antialias: false, bool alpha: false })
            .unwrap_unchecked()};
    let (_, _, envelope_ctx) = unsafe {
        utils::get_canvas_ctx("envelope",
            &utils::js_obj!{ bool alpha: false })
            .unwrap_unchecked()};
    main_ctx.set_stroke_style(&"#0069E1".into());
    main_ctx.set_fill_style(&"#181818".into());
    envelope_ctx.set_stroke_style(&"#0069E1".into());
    envelope_ctx.set_fill_style(&"#181818".into());
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
            envelope_graph: web_sys::Path2d::new()
                .expect_throw_val("initializing an empty envelope graph"),
            envelope_in_time: f64::NAN, envelope_span: f64::NAN,
            envelope_pbar_start: f64::NAN,
            solid_line: js_sys::Array::new().into(),
            dotted_line: js_sys::Array::of2(&(10.0).into(), &(10.0).into()).into(),
            renderer: render::Renderer::new(),
            js_callback: Default::default() // initialized later in `start_animation_loop`
        }));

        Self {help_msg: Self::DEF_DESC.into(),
            selected_comp: None, player}
    }

    fn update(&mut self, _: &yew::Context<Self>, msg: Self::Message) -> bool {
        let cur_time = self.player.current_time();
        match msg {
            MainCmd::SetDesc(value) =>
                self.help_msg = format!("{:1$}", value, Self::DEF_DESC.len()).into(),
            MainCmd::RemoveDesc() => 
                self.help_msg = Self::DEF_DESC.into(),
            MainCmd::Select(id) => if id.zip(std::mem::replace(&mut self.selected_comp, id)).is_some_and(|(x, y)| x == y) {
                self.selected_comp = None
            }
            MainCmd::TryConnect(src_id, x, y) => {
                if let Some(dst_id) = sound_comps().iter().position(|dst| dst.contains(x, y)) {
                    if let Ok([src, dst]) = sound_comps().get_many_mut([src_id, dst_id]) {
                        src.connect(dst)
                            .expect_throw_val("executing `MainCmd::TryConnect` command");
                    }
                }
            }
            MainCmd::SetParam(component_id, param_id, value) => {
                sound_comps().get_mut(component_id)
                    .expect_throw_with(|| format!("could not find component #{} to set one of its parameters", component_id))
                    .set_param(param_id, value, cur_time)
                    .expect_throw_val("setting the component's parameter")
            }
            MainCmd::Start() => {
                unsafe{ANIMATION_CTX.as_ref().unwrap_unchecked()}
                    .lock().expect_throw("failed to access the animation context")
                    .envelope_pbar_start = f64::INFINITY;
                for comp in sound_comps().iter_mut() {
                    comp.start(cur_time)
                        .expect_throw_val("starting a sound component")
                }
                return false}
            MainCmd::End() => {
                unsafe{ANIMATION_CTX.as_ref().unwrap_unchecked()}
                    .lock().expect_throw("failed to access the animation context")
                    .envelope_pbar_start = f64::NEG_INFINITY;
                for comp in sound_comps().iter_mut() {
                    comp.end(cur_time)
                        .expect_throw_val("ending a sound component")
                }
                return false}
        }
        true
    }

    fn view(&self, ctx: &yew::Context<Self>) -> yew::Html {
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
                    if let Some(selected) = self.selected_comp {
                        {sound_comps().get(selected)
                            .expect_throw("getting the selected component")
                            .params()}
                    }
                </div>
                <canvas id="envelope" class="graph"></canvas>
            </div>
            <div id="visuals">
                <canvas id="main" class="graph"></canvas>
                <button
                onmousedown={ctx.link().callback(|_| MainCmd::Start())}
                onmouseup={ctx.link().callback(|_| MainCmd::End())}>
                    {"Play"}
                </button>
            </div>
        </>}
    }

    fn rendered(&mut self, ctx: &yew::Context<Self>, first_render: bool) {
        if first_render {
            start_animation_loop();
            utils::sync_canvas("main");
            utils::sync_canvas("envelope");
            utils::document()
                .get_element_by_id("help-msg")
                .expect_throw("could not find the help message bar")
                .unchecked_into::<web_sys::HtmlElement>();
        }

        /*unsafe{ANIMATION_CTX.as_ref().unwrap_unchecked()}.lock().map(|mut handle| {
            let ctx = &mut *handle;
            let (width, height, _) = unsafe{utils::get_canvas_ctx("envelope", &Default::default())
                .unwrap_unchecked()};

            ctx.envelope_in_time = self.attack + self.decay;
            ctx.envelope_span = ctx.envelope_in_time + self.release;
            ctx.envelope_graph = web_sys::Path2d::new()
                .expect_throw_val("creating the envelope graph");
            ctx.envelope_graph.move_to(0.0, height);
            ctx.envelope_graph.line_to(self.attack / ctx.envelope_span * width, 0.0);
            ctx.envelope_graph.line_to(ctx.envelope_in_time / ctx.envelope_span * width,
                (1.0 - self.sustain) as f64 * height);
            ctx.envelope_graph.line_to(width, height);
        }).expect_throw("accessing the animation context");*/
    }
}

fn main() {
    yew::Renderer::<Main>::new().render();
}
