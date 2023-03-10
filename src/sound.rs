use crate::utils::{self, JsResultUtils, HitZone, SliceExt, BoolExt};
use crate::{input, MainCmd, Main};
use std::rc::Rc;

enum SoundFunctorType {
    Wave {n_waves: usize, waves: Vec<(usize, f32, usize, web_sys::OscillatorNode)>, ctrl: web_sys::GainNode},
    Envelope {
        gen: web_sys::GainNode,
        attack: f64,
        decay: f64,
        sustain: f64,
        release: f64},
    BuiltinOutput {gen: Rc<web_sys::AnalyserNode>}
}

#[derive(Debug)]
enum FocusType {
    None,
    Moving(utils::Point),
    Connecting(utils::Point),
}

pub struct SoundFunctor {
    functor_type: SoundFunctorType,
    id: usize,
    location: utils::Point,
    forwards: Vec<usize>,
    focus_type: FocusType
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
            SoundFunctorType::Wave {ref ctrl, ..} => ctrl,
            SoundFunctorType::Envelope {ref gen, ..} => gen,
            SoundFunctorType::BuiltinOutput {ref gen} => gen}
    }
}

impl SoundFunctor {
    pub const MAX_FREQ: f64 = 5000.0;
    pub const DEF_FREQ: f32 = 440.0;
    pub const MAX_INTERVAL: f64 = 2.0;
	pub const MIN_VOLUME: f32 = f32::MIN_POSITIVE;
    pub const VISUAL_SIZE: i32 = 32;
    pub const WAVE_TYPES: [web_sys::OscillatorType; 4] = [
        web_sys::OscillatorType::Sine,
        web_sys::OscillatorType::Square,
        web_sys::OscillatorType::Sawtooth,
        web_sys::OscillatorType::Triangle];

    pub fn new_wave(player: &web_sys::AudioContext, id: usize, location: utils::Point) -> utils::JsResult<Self> {
        let wave = web_sys::OscillatorNode::new(player)?;
        let ctrl = web_sys::GainNode::new_with_options(player, 
            web_sys::GainOptions::new().gain(Self::MIN_VOLUME))?;
        wave.start()?;
        wave.connect_with_audio_node(&ctrl)?;
        Ok(Self{functor_type: SoundFunctorType::Wave{n_waves: 1, waves: vec![(0, 440.0, 0, wave)], ctrl}, 
            id, location, forwards: vec![], focus_type: FocusType::None})
    }

    pub fn new_envelope(player: &web_sys::AudioContext, id: usize, location: utils::Point) -> utils::JsResult<Self> {
        let gen = web_sys::GainNode::new_with_options(player, 
            web_sys::GainOptions::new().gain(Self::MIN_VOLUME))?;
        Ok(Self{functor_type: SoundFunctorType::Envelope{gen, attack: 0.0, decay: 0.0, sustain: 0.0, release: 0.0},
            id, location, forwards: vec![], focus_type: FocusType::None})
    }

    pub fn new_builtin_output(gen: Rc<web_sys::AnalyserNode>, id: usize, location: utils::Point) -> utils::JsResult<Self> {
        Ok(Self{functor_type: SoundFunctorType::BuiltinOutput{gen},
            id, location, forwards: vec![], focus_type: FocusType::None})
    }

    pub fn start(&mut self, cur_time: f64) -> utils::JsResult<()> {
        match &self.functor_type {
            SoundFunctorType::Wave{waves, ctrl, ..} => {
                for (_, freq, wave_type, wave) in waves.iter() {
                    wave.frequency().set_value_at_time(*freq, cur_time)?;
                    wave.set_type(*Self::WAVE_TYPES.get_or_js_error(*wave_type, "invalid wave type #", "")?);
                }
                _ = ctrl.gain().cancel_scheduled_values(0.0)
                    .explain_err("resetting the wave generator to start it again")?
                    .set_value_at_time(1.0, cur_time)?}
            SoundFunctorType::Envelope{gen, attack, decay, sustain, ..}
                => _ = gen.gain().cancel_scheduled_values(0.0)
                .explain_err("resetting the volume control")?
                .linear_ramp_to_value_at_time(1.0, cur_time + attack)
                .explain_err("setting the attack period")?
                .linear_ramp_to_value_at_time(*sustain as f32,
                    cur_time + attack + decay)
                .explain_err("setting the decay period")?,
            _ => ()};
        Ok(())
    }

    pub fn end(&mut self, cur_time: f64, global_release_time: f64) -> utils::JsResult<()> {
        match &self.functor_type {
            SoundFunctorType::Wave{ctrl, ..}
                => _ = ctrl.gain().set_value_at_time(Self::MIN_VOLUME, cur_time + global_release_time)?,
            SoundFunctorType::Envelope{gen, release, ..}
                => _ = gen.gain().cancel_scheduled_values(0.0)
                .explain_err("resetting the envelope for the fade-out")?
                .linear_ramp_to_value_at_time(Self::MIN_VOLUME, cur_time + release)
                .explain_err("setting the volume fade-out")?,
            _ => ()};
        Ok(())
    }

    // the returned boolean signifies whether the component editor layout should be rerendered
    pub fn set_param(&mut self, param_id: usize, value: f64, ctx: &yew::html::Scope<Main>) -> utils::JsResult<bool> {
        match &mut self.functor_type {
            SoundFunctorType::Wave{n_waves, waves, ctrl} => {
                let (wave_id, param_id) = (param_id >> 2, param_id & 2);
                if wave_id == waves.len() {
                    if value.is_sign_negative() /*new wave only added after releasing the button for it*/{
                        let new_wave = web_sys::OscillatorNode::new_with_options(&ctrl.context(), 
                            web_sys::OscillatorOptions::new().frequency(Self::DEF_FREQ))?;
                        new_wave.connect_with_audio_node(ctrl)?;
                        new_wave.start()?;
                        waves.push((*n_waves, 440.0, 0, new_wave));
                        *n_waves += 1;
                        return Ok(true)}
                    return Ok(false)}

                let (_, freq, wave_type, _) = waves.get_mut_or_js_error(wave_id, "wave component #", " not found")?;
                match param_id {
                    0 => *freq = value as f32,
                    1 => *wave_type = value as usize,
                    2 => return Ok(value.is_sign_negative().then_try(|| waves.remove(wave_id).3.disconnect())?.is_some()),
                    _ => return Err(js_sys::Error::new("invalid parameter ID of `SoundFunctor::Wave`").into())
                }
            }

            SoundFunctorType::Envelope{attack, decay, sustain, release, ..} => match param_id {
                0 => *attack = value,
                1 => *decay = value,
                2 => *sustain = value,
                3 => {
                    ctx.send_message(MainCmd::SetGlobalReleaseTime(self.id, value));
                    *release = value}
                _ => return Err(js_sys::Error::new("invalid parameter ID of `SoundFunctor::Envelope`").into())}

            SoundFunctorType::BuiltinOutput{..}
                => return Err(js_sys::Error::new("cannot set a parameter on `SoundFunctor::BuiltinOutput`").into())
        };
        Ok(false)
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

    pub fn connect(&mut self, other: &SoundFunctor) -> utils::JsResult<bool> {
        if !other.backwardable() || self.forwards().filter(|x| !x.contains(&other.id)).is_none() {
            return Ok(false)}
        self.forwards.push(other.id);
        self.connect_with_audio_node(other)?;
        Ok(true)
    }

    pub fn disconnect(&mut self, other: &SoundFunctor) -> utils::JsResult<bool> {
        if !self.forwards.contains(&other.id) {return Ok(false)}
        self.disconnect_with_audio_node(other)?;
        self.forwards.retain(|&x| x != other.id);
        Ok(true)
    }

    pub fn handle_id_change(&mut self, from: usize, to: usize) {
        if self.id == from {
            self.id = to;
        } else {
            self.forwards.iter_mut().find(|x| **x == from).map(|x| *x = to);
        }
    }

    pub fn name(&self) -> &'static str {
        match &self.functor_type {
            SoundFunctorType::Wave{..} => "Wave generator",
            SoundFunctorType::Envelope{..} => "Envelope",
            SoundFunctorType::BuiltinOutput{..} => "Output"}
    }

    #[inline] pub fn id(&self) -> usize {self.id}

    pub fn contains(&self, point: utils::Point) -> bool {
        match (self.forwards().is_some(), self.backwardable()) {
            (true,  true) => utils::Rhombus::new(self.location, Self::VISUAL_SIZE, Self::VISUAL_SIZE)
                .contains(point),
            (false, false) => wasm_bindgen::throw_str("a sound functor must be either forwardable or backwardable"),
            (_, x) => utils::HorizontalArrow::new(self.location, Self::VISUAL_SIZE, Self::VISUAL_SIZE, x)
                .contains(point)
        }
    }

    pub fn params(&self) -> yew::Html {
        match &self.functor_type {
            SoundFunctorType::Wave{n_waves, waves, ..} => yew::html!{<div id="inputs" style="grid-template-columns:repeat(3,1fr)">
                {for waves.iter().enumerate().map(|(i, (key, freq, wave_type, _))| yew::html!{<>
                    <input::Slider
                    key={key * 3}
                    id={i * 4}
                    coef={SoundFunctor::MAX_FREQ} precision={0}
                    postfix={"Hz"}
                    name={"Frequency"}
                    component_id={self.id}
                    initial={*freq as f64}/>
                    <input::Switch
                    key={key * 3 + 1}
                    id={i * 4 + 1}
                    options={vec!["Sine", "Square", "Saw", "Triangle"]}
                    name={"Wave type"}
                    component_id={self.id}
                    initial={wave_type}/>
                    <input::Button
                    key={key * 3 + 2}
                    id={i * 4 + 2}
                    desc={"Remove wave element"}
                    component_id={self.id}>
                        <svg viewBox="0 0 100 100">
                            <polygon points="27,35 35,27 50,42 65,27 73,35 58,50 73,65 65,73 50,58 35,73 27,65 42,50"/>
                        </svg>
                    </input::Button>
                </>})}
                <input::Button
                key={n_waves * 3}
                id={waves.len() * 4}
                desc={"Add new wave element"}
                component_id={self.id}
                style={"grid-column: 1 / -1; width: auto"}>
                    <svg viewBox="0 0 100 100" style="height:100%">
                        <polygon points="45,25 55,25 55,45 75,45 75,55 55,55 55,75 45,75 45,55 25,55 25,45 45,45"/>
                    </svg>
                </input::Button>
            </div>},
            SoundFunctorType::Envelope{attack, decay, sustain, release, ..} => yew::html! {<div id="inputs">
                <input::Slider
                    id={0}
                    coef={SoundFunctor::MAX_INTERVAL}
                    postfix={"s"}
                    name={"Attack time"}
                    component_id={self.id}
                    initial={attack}/>
                <input::Slider
                    id={1}
                    coef={SoundFunctor::MAX_INTERVAL}
                    postfix={"s"}
                    name={"Decay time"}
                    component_id={self.id}
                    initial={decay}/>
                <input::Slider
                    id={2}
                    coef={1.0}
                    postfix={""}
                    name={"Sustain level"}
                    component_id={self.id}
                    initial={sustain}/>
                <input::Slider
                    id={3}
                    coef={SoundFunctor::MAX_INTERVAL}
                    postfix={"s"}
                    name={"Release time"}
                    component_id={self.id}
                    initial={release}/>
            </div>},
            SoundFunctorType::BuiltinOutput{..} => Default::default()}
    }

    pub fn draw(&self, ctx: &web_sys::CanvasRenderingContext2d, offset: utils::Point, others: &[Self]) -> utils::JsResult<()> {
        match (self.forwards().is_some(), self.backwardable()) {
            (false, false) => return Err("a sound functor must be either forwardable or backwardable".into()),
            (true,  true) => utils::Rhombus::new(self.location, Self::VISUAL_SIZE, Self::VISUAL_SIZE)
                .shift(-offset).draw(ctx),
            (_, x) => utils::HorizontalArrow::new(self.location, Self::VISUAL_SIZE, Self::VISUAL_SIZE, x)
                .shift(-offset).draw(ctx)
        }

        if let Some(fwds) = self.forwards() {
            let hitzone = self.get_conn_creation_hitzone().shift(-offset);
            hitzone.draw(ctx);
            for id in fwds {
                let comp = others.get_or_js_error(*id, "sound functor #", "not found")?;
                let dst = comp.location - offset;
                ctx.move_to(hitzone.right().into(), hitzone.center().y.into());
                ctx.line_to(dst.x.into(), dst.y.into());
            }
                
            if let FocusType::Connecting(mut conn) = self.focus_type {
                ctx.move_to(hitzone.right().into(), hitzone.center().y.into());
                conn -= offset;
                ctx.line_to(conn.x.into(), conn.y.into());
            }
        }
        Ok(ctx.stroke())
    }

    #[inline] fn get_conn_creation_hitzone(&self) -> impl utils::HitZone {
        utils::HorizontalArrow::new(self.location, Self::VISUAL_SIZE / 2, Self::VISUAL_SIZE / 2, false)
            .shift_x(Self::VISUAL_SIZE / 2)
    }

    pub fn handle_movement(&mut self, coords: Option<utils::Point>, ctx: &yew::html::Scope<Main>) -> utils::JsResult<()> {
        self.focus_type = if let Some(coords) = coords {
            match self.focus_type {
                FocusType::None => 
                    if self.forwards().is_some() && self.get_conn_creation_hitzone().contains(coords) {
                        ctx.send_message(MainCmd::SetDesc(format!("{}: Connecting", self.name())));
                        FocusType::Connecting(coords)
                    } else {
                        ctx.send_message(MainCmd::SetDesc(format!("{}: Moving", self.name())));
                        FocusType::Moving(self.location - coords)
                    }

                FocusType::Moving(offset) => {
                    self.location = coords + offset;
                    FocusType::Moving(offset)}

                FocusType::Connecting(_) => FocusType::Connecting(coords)
            }
        } else {
            match self.focus_type {
                FocusType::Connecting(dst) => ctx.send_message_batch(vec![
                    MainCmd::TryConnect(self.id, dst),
                    MainCmd::Select(self.id)]),
                FocusType::Moving(_) => ctx.send_message(MainCmd::Select(self.id)),
                FocusType::None => ()};
            FocusType::None};
        Ok(())
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
