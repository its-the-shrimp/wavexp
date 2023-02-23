use crate::utils::{self, JsResultUtils, Tee, ToJsResult, OkOrJsError};
use crate::{input, get_sound_comp, draggable, PLANE_OFFSET};
use crate::{SOUND_COMPS, MainCmd};
use std::rc::Rc;

pub enum SoundFunctorType {
    Wave {gen: web_sys::OscillatorNode},
    Envelope {
        gen: web_sys::GainNode,
        attack: f64,
        decay: f64,
        sustain: f64,
        release: f64},
    BuiltinOutput {gen: Rc<web_sys::AnalyserNode>}
}

pub struct SoundFunctor {
    functor_type: SoundFunctorType,
    id: usize,
    location: [i32; 2],
    forwards: Vec<usize>,
    new_conn: Option<[i32; 2]>
}

impl PartialEq for SoundFunctor {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for SoundFunctor {}

impl std::ops::Deref for SoundFunctor {
    type Target = web_sys::AudioNode;
    fn deref(&self) -> &Self::Target {
        match self.functor_type {
            SoundFunctorType::Wave {ref gen} => gen,
            SoundFunctorType::Envelope {ref gen, ..} => gen,
            SoundFunctorType::BuiltinOutput {ref gen} => gen}
    }
}

impl SoundFunctor {
    pub const MAX_FREQ: f64 = 5000.0;
    pub const DEF_FREQ: f32 = 440.0;
    pub const MAX_INTERVAL: f64 = 2.0;
	pub const MAX_VOLUME: f32 = 0.2;
	pub const MIN_VOLUME: f32 = f32::MIN_POSITIVE;
    pub const VISUAL_SIZE: i32 = 32;

    pub fn new_wave(player: &web_sys::AudioContext, id: usize, x: i32, y: i32) -> utils::JsResult<Self> {
        let gen = web_sys::OscillatorNode::new_with_options(player, 
            web_sys::OscillatorOptions::new().frequency(Self::DEF_FREQ))?;
        gen.start()?;
        Ok(Self{functor_type: SoundFunctorType::Wave{gen}, 
            id, location: [x, y], forwards: vec![], new_conn: None})
    }

    pub fn new_envelope(player: &web_sys::AudioContext, id: usize, x: i32, y: i32) -> utils::JsResult<Self> {
        let gen = web_sys::GainNode::new_with_options(player, 
            web_sys::GainOptions::new().gain(Self::MIN_VOLUME))?;
        Ok(Self{functor_type: SoundFunctorType::Envelope{gen, attack: 0.0, decay: 0.0, sustain: 0.0, release: 0.0},
            id, location: [x, y], forwards: vec![], new_conn: None})
    }

    pub fn new_builtin_output(gen: Rc<web_sys::AnalyserNode>, id: usize, x: i32, y: i32) -> utils::JsResult<Self> {
        Ok(Self{functor_type: SoundFunctorType::BuiltinOutput{gen},
            id, location: [x, y], forwards: vec![], new_conn: None})
    }

    pub fn start(&mut self, cur_time: f64) -> utils::JsResult<()> {
        match &self.functor_type {
            SoundFunctorType::Envelope{gen, attack, decay, sustain, ..}
                => _ = gen.gain().cancel_scheduled_values(0.0)
                .explain_err("resetting the volume control")?
                .linear_ramp_to_value_at_time(Self::MAX_VOLUME, cur_time + attack)
                .explain_err("setting the attack period")?
                .linear_ramp_to_value_at_time(Self::MAX_VOLUME * *sustain as f32,
                    cur_time + attack + decay)
                .explain_err("setting the decay period")?,
            _ => ()};
        Ok(())
    }

    pub fn end(&mut self, cur_time: f64) -> utils::JsResult<()> {
        match &self.functor_type {
            SoundFunctorType::Envelope{gen, release, ..}
                => _ = gen.gain().cancel_scheduled_values(0.0)
                .explain_err("resetting the envelope for the fade-out")?
                .linear_ramp_to_value_at_time(Self::MIN_VOLUME, cur_time + release)
                .explain_err("setting the volume fade-out")?,
            _ => ()};
        Ok(())
    }

    pub fn set_param(&mut self, param_id: usize, value: f64, cur_time: f64) -> utils::JsResult<()> {
        match &mut self.functor_type {
            SoundFunctorType::Wave {gen} => match param_id {
                0 => Ok(_ = gen.frequency().set_value_at_time(value as f32, cur_time)?
                    .value().js_log("new frequency: ")),
                1 => match value as usize {
                    0 => Ok(gen.set_type(web_sys::OscillatorType::Sine)),
                    1 => Ok(gen.set_type(web_sys::OscillatorType::Square)),
                    2 => Ok(gen.set_type(web_sys::OscillatorType::Sawtooth)),
                    3 => Ok(gen.set_type(web_sys::OscillatorType::Triangle)),
                    _ => Err(js_sys::Error::new("invalid wave type").into())}
                _ => Err(js_sys::Error::new("invalid parameter ID of `SoundFunctor::Wave`").into())}
            SoundFunctorType::Envelope {attack, decay, sustain, release, ..} => match param_id {
                0 => Ok(*attack = value),
                1 => Ok(*decay = value),
                2 => Ok(*sustain = value),
                3 => Ok(*release = value),
                _ => Err(js_sys::Error::new("invalid parameter ID of `SoundFunctor::Envelope`").into())}
            SoundFunctorType::BuiltinOutput{..}
                => Err(js_sys::Error::new("cannot set a parameter on `SoundFunctor::BuiltinOutput`").into())}
    }

    #[inline] pub fn forwards<'a>(&'a self) -> Option<&'a [usize]> {
        match self.functor_type {
            SoundFunctorType::BuiltinOutput{..} => None,
            _ => Some(&*self.forwards)}
    }

    #[inline] pub fn backwardable(&self) -> bool {
        match self.functor_type {
            SoundFunctorType::Wave {..} => false,
            _ => true}
    }

    pub fn connect(&mut self, other: &SoundFunctor) -> utils::JsResult<Option<()>> {
        if !other.backwardable() || self.forwards().filter(|x| !x.contains(&other.id)).is_none() {return Ok(None)}
        self.forwards.push(other.id);
        self.connect_with_audio_node(other)?;
        Ok(Some(()))
    }

    pub fn disconnect(&mut self, other: &SoundFunctor) -> utils::JsResult<Option<()>> {
        if !self.forwards.contains(&other.id) {return Ok(None)}
        self.disconnect_with_audio_node(other)?;
        self.forwards.retain(|&x| x != other.id);
        Ok(Some(()))
    }

    pub fn name(&self) -> &'static str {
        match &self.functor_type {
            SoundFunctorType::Wave{..} => "Wave generator",
            SoundFunctorType::Envelope{..} => "Envelope",
            SoundFunctorType::BuiltinOutput{..} => "Output"}
    }

    #[inline] pub fn id(&self) -> usize {self.id}
    #[inline] pub fn location(&self) -> [i32; 2] {self.location}

    pub fn contains(&self, x: i32, y: i32) -> bool {
        (self.location[0] - Self::VISUAL_SIZE ..= self.location[0] + Self::VISUAL_SIZE).contains(&x)
        && (self.location[1] - Self::VISUAL_SIZE ..= self.location[1] + Self::VISUAL_SIZE).contains(&y)
    }

    pub fn params(&self) -> yew::Html {
        match &self.functor_type {
            SoundFunctorType::Wave{gen} => yew::html!{<>
                <input::Slider
                    id={0}
                    coef={SoundFunctor::MAX_FREQ} precision={0}
                    postfix={"Hz"}
                    name={Rc::from("Frequency")}
                    component_id={self.id}
                    initial={gen.frequency().value() as f64}/>
                <input::Switch
                    id={1}
                    options={vec!["Sine".into(), "Square".into(), "Saw".into(), "Triangle".into()]}
                    name={Rc::from("Wave type")}
                    component_id={self.id}
                    initial={match gen.type_() {
                        web_sys::OscillatorType::Sine => 0,
                        web_sys::OscillatorType::Square => 1,
                        web_sys::OscillatorType::Sawtooth => 2,
                        web_sys::OscillatorType::Triangle => 3,
                        _ => wasm_bindgen::throw_str("found an invalid wave type while getting it to generate component's parameter list")}}/>
            </>},
            SoundFunctorType::Envelope{attack, decay, sustain, release, ..} => yew::html! {<>
                <input::Slider
                    id={0}
                    coef={SoundFunctor::MAX_INTERVAL}
                    postfix={"s"}
                    name={Rc::from("Attack time")}
                    component_id={self.id}
                    initial={attack}/>
                <input::Slider
                    id={1}
                    coef={SoundFunctor::MAX_INTERVAL}
                    postfix={"s"}
                    name={Rc::from("Decay time")}
                    component_id={self.id}
                    initial={decay}/>
                <input::Slider
                    id={2}
                    coef={1.0}
                    name={Rc::from("Sustain level")}
                    component_id={self.id}
                    initial={sustain}/>
                <input::Slider
                    id={3}
                    coef={SoundFunctor::MAX_INTERVAL}
                    postfix={"s"}
                    name={Rc::from("Release time")}
                    component_id={self.id}
                    initial={release}/>
            </>},
            SoundFunctorType::BuiltinOutput{..} => Default::default()}
    }

    pub fn draw(&self, ctx: &web_sys::CanvasRenderingContext2d, offset: [i32; 2]) -> utils::JsResult<()> {
        let x = (self.location[0] - offset[0]) as f64;
        let y = (self.location[1] - offset[1]) as f64;
        ctx.move_to(x, y - Self::VISUAL_SIZE as f64);
        if let Some(fwds) = self.forwards() {
            ctx.line_to(x + Self::VISUAL_SIZE as f64, y as f64);
            let comps = SOUND_COMPS.try_borrow().to_js_result()?;
            for id in fwds {
                let comp = comps.get(*id)
                    .ok_or_js_error_with(||
                        format!("error while rendering connections to other components: sound functor #{} not found", id))?;
                let [x2, y2] = comp.location();
                ctx.line_to((x2 - offset[0]) as f64, (y2 - offset[1]) as f64);
                ctx.move_to(x + Self::VISUAL_SIZE as f64, y as f64);
            }
                
        }
        ctx.line_to(x, y + Self::VISUAL_SIZE as f64);
        if self.backwardable() {
            ctx.line_to(x - Self::VISUAL_SIZE as f64, y as f64)}
        ctx.line_to(x, y - Self::VISUAL_SIZE as f64);
        if let Some(conn) = self.new_conn {
            ctx.move_to(x + Self::VISUAL_SIZE as f64, y);
            ctx.line_to(conn[0] as f64, conn[1] as f64);
        }
        ctx.stroke();
        Ok(())
    }

    #[inline] pub fn handle_movement(&mut self, coords: Option<[i32; 2]>) -> utils::JsResult<()> {
        Ok(self.new_conn = coords.filter(|_| self.forwards().is_some()))
    }

    pub fn graph(&self, width: f64, height: f64) -> utils::JsResult<(web_sys::Path2d, f64, f64)> {
        match &self.functor_type {
            SoundFunctorType::Envelope {attack, decay, sustain, release, ..} => {
                let res = (web_sys::Path2d::new()?, attack + decay);
                let res = (res.0, res.1, res.1 + release);
                res.0.move_to(0.0, height);
                res.0.line_to(attack / res.2 * width, 0.0);
                res.0.line_to(res.1 / res.2 * width, (1.0 - sustain) as f64 * height);
                res.0.line_to(width, height);
                Ok(res)}
            _ => Ok((web_sys::Path2d::new()?, f64::NAN, f64::NAN))
        }
    }

    pub fn graphable(&self) -> bool {
        match &self.functor_type {
            SoundFunctorType::Envelope {..} => true,
            _ => false
        }
    }
}
