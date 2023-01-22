mod render;
mod utils;
mod input;
use input::{Slider, Switch};
use utils::{ExpectThrowVal, Pipe};
use web_sys;
use js_sys;
use wasm_bindgen;
use wasm_bindgen::{UnwrapThrowExt, JsCast};
use std::rc::Rc;

struct AnimationCtx {
    analyser: web_sys::AnalyserNode,
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
            let buf = ctx.renderer.set_size(main_width, main_height).get_in_buffer();
            ctx.analyser.get_byte_frequency_data(buf);
            main_ctx.put_image_data(
                &web_sys::ImageData::new_with_u8_clamped_array(
                    wasm_bindgen::Clamped(ctx.renderer.graph().get_out_bytes()),
                    main_width as u32)
                    .expect_throw_val("preparing the rendered audio visualisation"),
                0.0, 0.0)
                .expect_throw_val("outputting the rendered audio visualisation");
            
            envelope_ctx.fill_rect(0.0, 0.0, envelope_width as f64, envelope_height as f64);
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
                } / ctx.envelope_span * envelope_width as f64;
                envelope_ctx.begin_path();
                envelope_ctx.move_to(x, 0.0);
                envelope_ctx.line_to(x, envelope_height as f64);
                envelope_ctx.stroke();
            }

            utils::window()
                .request_animation_frame(&ctx.js_callback)
                .expect_throw_val("rescheduling the animation callback");
        }).expect_throw("getting the animation context from the animation loop");
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
	attack: f64,
    decay: f64,
    sustain: f64,
	release: f64,
	player: web_sys::AudioContext,
	source: web_sys::OscillatorNode,
	envelope: web_sys::GainNode,
    help_msg: Rc<str>
}

#[derive(Debug)]
enum MainCmd {
	SetFreq(f64),
	SetAttack(f64),
    SetDecay(f64),
    SetSustain(f64),
	SetRelease(f64),
    SetDesc(Rc<str>),
    RemoveDesc(()),
    SetWaveType(u8),
	Start(), 
	End(),
    Redraw()
}

impl Main {
	const CHANNEL_COUNT: u32 = 2;
    const MAX_FREQ: f64 = 5000.0;
    const MAX_INTERVAL: f64 = 2.0;
	const MAX_VOLUME: f32 = 0.2;
	const MIN_VOLUME: f32 = f32::MIN_POSITIVE;
    const DEF_DESC: &'static str = "Here will be a help message";
}

static mut REDRAW_HOOK: Option<yew::Callback<()>> = None;

impl yew::Component for Main {
	type Message = MainCmd;
	type Properties = ();

	fn create(ctx: &yew::Context<Self>) -> Self {
		let player = web_sys::AudioContext::new()
			.expect_throw_val("initialising the audio context");
		let envelope = web_sys::GainNode::new_with_options(
			&player,
			web_sys::GainOptions::new()
				.channel_count(Self::CHANNEL_COUNT)
				.gain(Self::MIN_VOLUME))
			.expect_throw_val("initialising the volume controller");
		let source = web_sys::OscillatorNode::new_with_options(
			&player, web_sys::OscillatorOptions::new()
				.channel_count(Self::CHANNEL_COUNT)
				.frequency(0.0))
			.expect_throw_val("initializing the audio source");
        let analyser = web_sys::AnalyserNode::new(&player)
            .expect_throw_val("initialising the audio analyser");
		source.connect_with_audio_node(&envelope)
			.expect_throw_val("connecting the volume controller")
			.connect_with_audio_node(&analyser)
			.expect_throw_val("connecting the audio analyser")
			.connect_with_audio_node(&player.destination())
			.expect_throw_val("connecting the audio output");
		source.start().expect_throw_val("starting the audio");
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
// initializing the resize callback to adjust the canvases
        *unsafe{&mut REDRAW_HOOK} = Some(ctx.link().callback(|()| MainCmd::Redraw()));
        utils::window().set_onresize(Some(&wasm_bindgen::closure::Closure::<dyn Fn(web_sys::Event)>::new(|_| {
            let iter = utils::document().query_selector_all("canvas")
                .expect_throw_val("selecting all the canvases after resizing the screen");
            for i in 0..iter.length() {
                let canvas = unsafe{iter.get(i).unwrap_unchecked()} // this is safe because `i` is
                    .unchecked_into::<web_sys::HtmlCanvasElement>(); // guaranteed to be in the range `[0; iter.length())`
                let ctx = canvas.get_context("2d")
                    .expect_throw_val("getting the rendering context of one of the canvases after resizing the screen")
                    .expect_throw("getting the rendering context of one of the canvases after resizing the screen")
                    .unchecked_into::<web_sys::CanvasRenderingContext2d>();
                let (fill_style, stroke_style, line_width, text_align, text_baseline) = 
                    (ctx.fill_style(),
                     ctx.stroke_style(),
                     ctx.line_width(),
                     ctx.text_align(),
                     ctx.text_baseline());
                canvas.set_height((300.0 * canvas.client_height() as f64 / canvas.client_width() as f64) as u32);
                ctx.set_font("2em consolas");
                ctx.set_fill_style(&fill_style);
                ctx.set_stroke_style(&stroke_style);
                ctx.set_line_width(line_width);
                ctx.set_text_align(&text_align);
                ctx.set_text_baseline(&text_baseline);
            }
            unsafe{REDRAW_HOOK.as_ref().unwrap_unchecked()} // this is safe because by the time the handler
                .emit(())                                   // is registered, `REDRAW_HOOK` is already defined
        }).into_js_value().unchecked_into::<js_sys::Function>()));

		Self {help_msg: Self::DEF_DESC.into(),
            attack: 0.0, release: 0.0,
            decay: 0.0, sustain: 0.5,
			source, envelope, player}
	}

	fn update(&mut self, _: &yew::Context<Self>, msg: Self::Message) -> bool {
        let cur_time = self.player.current_time();
		match msg {
			MainCmd::SetFreq(value) => {
                self.source.frequency()
                    .set_value_at_time(value as f32, cur_time)
                    .expect_throw_val("updating the frequency");
                false}
			MainCmd::SetAttack(value)  => {self.attack   = value;                 true}
            MainCmd::SetDecay(value)   => {self.decay    = value;                 true}
            MainCmd::SetSustain(value) => {self.sustain  = value;                 true}
			MainCmd::SetRelease(value) => {self.release  = value;                 true}
            MainCmd::SetDesc(value)    => {self.help_msg = value;                 true}
            MainCmd::RemoveDesc(())    => {self.help_msg = Self::DEF_DESC.into(); true}
            MainCmd::SetWaveType(id)   => {
                self.source.set_type(
                    [web_sys::OscillatorType::Sine,
                        web_sys::OscillatorType::Square,
                        web_sys::OscillatorType::Sawtooth,
                        web_sys::OscillatorType::Triangle][id as usize]);
                false}
			MainCmd::Start() => {
                unsafe{ANIMATION_CTX.as_ref().unwrap_unchecked()}
                    .lock().expect_throw("accessing the animation context")
                    .envelope_pbar_start = f64::INFINITY;
                self.envelope.gain()
                    .cancel_scheduled_values(0.0)
                    .expect_throw_val("resetting the volume control")
                    .linear_ramp_to_value_at_time(Self::MAX_VOLUME,
                        cur_time + self.attack)
                    .expect_throw_val("setting the attack period")
                    .linear_ramp_to_value_at_time(Self::MAX_VOLUME * self.sustain as f32,
                        cur_time + self.attack + self.decay)
                    .expect_throw_val("setting the decay period");
                false}
			MainCmd::End() => {
                unsafe{ANIMATION_CTX.as_ref().unwrap_unchecked()}
                    .lock().expect_throw("accessing the animation context")
                    .envelope_pbar_start = f64::NEG_INFINITY;
                self.envelope.gain()
                    .cancel_scheduled_values(0.0)
                    .expect_throw_val("resetting the envelope for the fade-out")
                    .linear_ramp_to_value_at_time(Self::MIN_VOLUME,
                        cur_time + self.release)
                    .expect_throw_val("setting the volume fade-out");
                false}
            MainCmd::Redraw() => true
		}
	}

	fn view(&self, ctx: &yew::Context<Self>) -> yew::Html {
		yew::html! {<div id="main-grid">
            <div id="component-menu">{"Nothing here yet"}</div>
			<div id="ctrl-panel">
                <div>{self.help_msg.clone()}</div>
                <div id="inputs">
                    <Slider
                        coef={Self::MAX_FREQ} precision={0}
                        postfix={"Hz"}
                        onfocus={ctx.link().callback(|()| MainCmd::SetDesc("Frequency".into()))}
                        onunfocus={ctx.link().callback(MainCmd::RemoveDesc)}
                        oninput={ctx.link().callback(MainCmd::SetFreq)}/>
                    <Slider
                        coef={Self::MAX_INTERVAL}
                        postfix={"s"}
                        onfocus={ctx.link().callback(|()| MainCmd::SetDesc("Attack time".into()))} 
                        onunfocus={ctx.link().callback(MainCmd::RemoveDesc)} 
                        oninput={ctx.link().callback(MainCmd::SetAttack)}/>
                    <Slider
                        coef={Self::MAX_INTERVAL}
                        postfix={"s"}
                        onfocus={ctx.link().callback(|()| MainCmd::SetDesc("Decay time".into()))} 
                        onunfocus={ctx.link().callback(MainCmd::RemoveDesc)} 
                        oninput={ctx.link().callback(MainCmd::SetDecay)}/>
                    <Slider
                        onfocus={ctx.link().callback(|()| MainCmd::SetDesc("Sustain level".into()))} 
                        onunfocus={ctx.link().callback(MainCmd::RemoveDesc)} 
                        oninput={ctx.link().callback(MainCmd::SetSustain)}/>
                    <Slider
                        coef={Self::MAX_INTERVAL}
                        postfix={"s"}
                        onfocus={ctx.link().callback(|()| MainCmd::SetDesc("Release time".into()))} 
                        onunfocus={ctx.link().callback(MainCmd::RemoveDesc)} 
                        oninput={ctx.link().callback(MainCmd::SetRelease)}/>
                    <Switch
                        options={vec!["Sine".into(), "Square".into(), "Saw".into(), "Triangle".into()]}
                        onfocus={ctx.link().callback(|()| MainCmd::SetDesc("Wave type".into()))}
                        onunfocus={ctx.link().callback(|()| MainCmd::RemoveDesc(()))}
                        oninput={ctx.link().callback(MainCmd::SetWaveType)}/>
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
            <div id="component-map" class="graph">{"Nothing here yet"}</div>
		</div>}
	}

	fn rendered(&mut self, ctx: &yew::Context<Self>, first: bool) {
        if first {
            unsafe{utils::window().onresize().unwrap_unchecked()}
                .call0(&wasm_bindgen::JsValue::UNDEFINED)
                .expect_throw_val("emitting the first resize event to prepare the canvases");
            start_animation_loop();
            ctx.link().send_message(MainCmd::Redraw());
        }

        unsafe{ANIMATION_CTX.as_ref().unwrap_unchecked()}.lock().map(|mut handle| {
            let ctx = &mut *handle;
            let (width, height) = unsafe{utils::document()
                .get_element_by_id("envelope").unwrap_unchecked()}
                .unchecked_into::<web_sys::HtmlCanvasElement>()
                .pipe(|x| (x.client_width() as f64, x.client_height() as f64));

            ctx.envelope_in_time = self.attack + self.decay;
            ctx.envelope_span = ctx.envelope_in_time + self.release;
            ctx.envelope_graph = web_sys::Path2d::new()
                .expect_throw_val("creating the envelope graph");
            ctx.envelope_graph.move_to(0.0, height);
            ctx.envelope_graph.line_to(self.attack / ctx.envelope_span * width, 0.0);
            ctx.envelope_graph.line_to(ctx.envelope_in_time / ctx.envelope_span * width,
                (1.0 - self.sustain) as f64 * height);
            ctx.envelope_graph.line_to(width, height);
        }).expect_throw("accessing the animation context");
    }
}

fn main() {
    yew::Renderer::<Main>::new().render();
}
