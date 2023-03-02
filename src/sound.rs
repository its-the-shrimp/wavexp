use crate::utils::{self, JsResultUtils, HitZone, SliceExt};
use crate::{input, MainCmd, Main};
use std::rc::Rc;

enum SoundFunctorType {
    Wave {gen: web_sys::OscillatorNode},
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

    pub fn new_wave(player: &web_sys::AudioContext, id: usize, location: utils::Point) -> utils::JsResult<Self> {
        let gen = web_sys::OscillatorNode::new_with_options(player, 
            web_sys::OscillatorOptions::new().frequency(Self::DEF_FREQ))?;
        gen.start()?;
        Ok(Self{functor_type: SoundFunctorType::Wave{gen}, 
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

    pub fn set_param(&mut self, param_id: usize, value: f64, cur_time: f64) -> utils::JsResult<&mut Self> {
        match &mut self.functor_type {
            SoundFunctorType::Wave {gen} => match param_id {
                0 => _ = gen.frequency().set_value_at_time(value as f32, cur_time)?.value(),
                1 => match value as usize {
                    0 => gen.set_type(web_sys::OscillatorType::Sine),
                    1 => gen.set_type(web_sys::OscillatorType::Square),
                    2 => gen.set_type(web_sys::OscillatorType::Sawtooth),
                    3 => gen.set_type(web_sys::OscillatorType::Triangle),
                    _ => return Err(js_sys::Error::new("invalid wave type").into())}
                _ => return Err(js_sys::Error::new("invalid parameter ID of `SoundFunctor::Wave`").into())}

            SoundFunctorType::Envelope {attack, decay, sustain, release, ..} => match param_id {
                0 => *attack = value,
                1 => *decay = value,
                2 => *sustain = value,
                3 => *release = value,
                _ => return Err(js_sys::Error::new("invalid parameter ID of `SoundFunctor::Envelope`").into())}

            SoundFunctorType::BuiltinOutput{..}
                => return Err(js_sys::Error::new("cannot set a parameter on `SoundFunctor::BuiltinOutput`").into())
        };
        Ok(self)
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
    #[inline] pub fn location(&self) -> utils::Point {self.location}

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
            SoundFunctorType::Wave{gen} => yew::html!{<>
                <input::Slider
                    id={0}
                    coef={SoundFunctor::MAX_FREQ} precision={0}
                    postfix={"Hz"}
                    name={"Frequency"}
                    component_id={self.id}
                    initial={gen.frequency().value() as f64}/>
                <input::Switch
                    id={1}
                    options={vec!["Sine", "Square", "Saw", "Triangle"]}
                    name={"Wave type"}
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
            </>},
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
                let dst = comp.location() - offset;
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

    #[inline] pub fn get_conn_creation_hitzone(&self) -> impl utils::HitZone {
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
