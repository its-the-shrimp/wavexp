use crate::utils::{self, JsResultUtils, HitZone, SliceExt, Tee};
use crate::{input, MainCmd, Main};
use std::rc::Rc;

#[derive(Clone, Copy)]
enum Tone {Freq(f32), Pitch(u32)}

impl Tone {
    #[inline] fn freq(self) -> f32 {
        match self {
            Tone::Freq(x) => x,
            Tone::Pitch(x) => unsafe{*Wave::NOTE_FREQS.get_unchecked(x as usize)}
        }
    }

    #[inline] fn pitch(self) -> usize {
        match self {
            Tone::Freq(x) => {
                for (index, &freq) in Wave::NOTE_FREQS.iter().enumerate() {
                    if x < freq {return index.saturating_sub(1)}
                }
                Wave::NOTE_FREQS.len() - 1
            }
            Tone::Pitch(x) => x as usize
        }
    }

    #[inline] fn set(&mut self, val: f64) -> bool {
        const MAX: u32 = Wave::NOTE_FREQS.len() as u32 - 1;
        match self {
            Tone::Freq(x) if x.is_finite() => *x = val as f32,
            Tone::Pitch(x @ ..=MAX)        => *x = val as u32,
            _ => return false}
        true
    }

    #[inline] fn toggle(&mut self) {
        match self {
            x@Tone::Freq(_) => *x = Tone::Pitch(x.pitch() as u32),
            x@Tone::Pitch(_) => *x = Tone::Freq(x.freq())
        }
    }
}

struct Wave {
    id: usize,
    tone: Tone,
    wave_type: usize,
    gen: web_sys::OscillatorNode,
    shift: u8,
    shifter: web_sys::DelayNode
}

impl Drop for Wave {
    fn drop(&mut self) {
        _ = self.gen.disconnect();
        _ = self.shifter.disconnect();
    }
}

impl Wave {
    pub const MAX_FREQ: f64 = 5000.0;
    pub const WAVE_TYPES: [web_sys::OscillatorType; 4] = [
        web_sys::OscillatorType::Sine,
        web_sys::OscillatorType::Square,
        web_sys::OscillatorType::Sawtooth,
        web_sys::OscillatorType::Triangle];
    pub const WAVE_TYPE_NAMES: [&'static str; 4] = [
        "Sine",
        "Square",
        "Saw",
        "Triangle"];
    pub const NOTE_FREQS: [f32; 24] = [
        55.00 /*A1*/, 58.27 /*A#1*/,
        61.74 /*B1*/,
        65.41 /*C2*/, 69.30 /*C#2*/,
        73.42 /*D2*/, 77.78 /*D#2*/,
        82.41 /*E2*/,
        87.31 /*F2*/, 92.50 /*F#2*/,
        98.00 /*G2*/, 103.83/*G#2*/,
        110.0 /*A2*/, 116.54/*A#2*/,
        123.47/*B2*/,
        130.81/*C3*/, 138.59/*C#3*/,
        146.83/*D3*/, 155.56/*D#3*/,
        164.81/*E3*/,
        174.61/*F3*/, 185.00/*F#3*/,
        196.00/*G3*/, 207.65/*G#3*/];
    pub const NOTE_NAMES: [&'static str; 24] = [
        "A1", "A#1",
        "B1",
        "C2", "C#2",
        "D2", "D#2",
        "E2",
        "F2", "F#2",
        "G2", "G#2",
        "A2", "A#2",
        "B2",
        "C3", "C#3",
        "D3", "D#3",
        "E3",
        "F3", "F#3",
        "G3", "G#3"];

    pub fn new(id: usize, ctrl: &web_sys::GainNode) -> utils::JsResult<Self> {
        let ctx = ctrl.context();
        let gen = web_sys::OscillatorNode::new(&ctx)?;
        let shifter = web_sys::DelayNode::new(&ctx)?;
        gen.connect_with_audio_node(&shifter)?;
        gen.start()?;
        shifter.connect_with_audio_node(ctrl)?;
        Ok(Self{id, tone: Tone::Pitch(0), wave_type: 0, gen, shift: 0, shifter})
    }

    pub fn prepare(&mut self, cur_time: f64) -> utils::JsResult<()> {
        unsafe {
            let freq = self.tone.freq();
            self.gen.frequency().set_value_at_time(freq, cur_time)?;
            self.gen.set_type(*Self::WAVE_TYPES.get_unchecked(self.wave_type));
            let shift = if freq == 0.0 {0.0} else {1.0 / freq * self.shift as f32 / 100.0};
            self.shifter.delay_time().set_value_at_time(shift, cur_time)?;
            Ok(())
        }
    }

    #[inline] pub fn id(&self) -> usize {self.id}
    #[inline] pub fn tone(&self) -> Tone {self.tone}
    #[inline] pub fn set_tone(&mut self, val: f64) -> utils::JsResult<()> {
        self.tone.set(val).then_some(())
            .ok_or_else(|| format!("invalid tone value {}", val).into())
    }
    #[inline] pub fn toggle_tone_input_mode(&mut self) {
        self.tone.toggle()
    }
    #[inline] pub fn wave_type(&self) -> usize {self.wave_type}
    #[inline] pub fn set_wave_type(&mut self, val: usize) -> utils::JsResult<()> {
        if val >= Self::WAVE_TYPES.len() {return Err(format!("invalid wave type #{}", val).into())}
        Ok(self.wave_type = val)
    }
    #[inline] pub fn shift(&self) -> f64 {self.shift as f64 / 100.0}
    #[inline] pub fn set_shift(&mut self, val: f64) {self.shift = (val * 100.0).min(99.9) as u8}
}

enum SoundFunctorType {
    Wave {
        n_waves: usize, // for unique wave ID generation
        waves: Vec<Wave>,
        ctrl: web_sys::GainNode},
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
    pub const MAX_INTERVAL: f64 = 2.0;
	pub const MIN_VOLUME: f32 = f32::MIN_POSITIVE;
    pub const VISUAL_SIZE: i32 = 32;

    pub fn new_wave(player: &web_sys::AudioContext, id: usize, location: utils::Point) -> utils::JsResult<Self> {
        let ctrl = web_sys::GainNode::new_with_options(player, 
            web_sys::GainOptions::new().gain(Self::MIN_VOLUME))?;
        Ok(Self{
            functor_type: SoundFunctorType::Wave{n_waves: 1, waves: vec![Wave::new(0, &ctrl)?], ctrl}, 
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
        match &mut self.functor_type {
            SoundFunctorType::Wave{waves, ctrl, ..} => {
                waves.len().js_log("waves: ");
                waves.iter_mut().try_for_each(|x| x.prepare(cur_time))?;
                _ = ctrl.gain().cancel_scheduled_values(0.0)
                    .explain_err("resetting the wave generator to start it again")?
                    .set_value_at_time(1.0, cur_time)?}
            SoundFunctorType::Envelope{gen, attack, decay, sustain, ..}
                => _ = gen.gain().cancel_scheduled_values(0.0)
                .explain_err("resetting the volume control")?
                .linear_ramp_to_value_at_time(1.0, cur_time + *attack)
                .explain_err("setting the attack period")?
                .linear_ramp_to_value_at_time(*sustain as f32,
                    cur_time + *attack + *decay)
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
                let (wave_id, param_id) = (param_id / 5, param_id % 5);
                if wave_id == waves.len() {
                    if param_id != 0 {return Err("invalid parameter ID of `SoundFunctor::Wave`".into())}
                    if value.is_sign_positive() {return Ok(false)}
                    waves.push(Wave::new(*n_waves, ctrl)?);
                    *n_waves += 1;
                    return Ok(true)
                } else {
                    let wave = waves.get_mut_or_js_error(wave_id, "wave component #", " not found")?;
                    match param_id {
                        0 => wave.set_tone(value)?,
                        1 => wave.set_wave_type(value as usize)?,
                        2 => return Ok(value.is_sign_negative().then(||
                            waves.remove(wave_id.js_log("id: "))).is_some()),
                        3 => return Ok(value.is_sign_negative().then(||
                            wave.toggle_tone_input_mode()).is_some()),
                        4 => wave.set_shift(value),
                        x => return Err(format!("invalid parameter ID of `SoundFunctor::Wave` {}", x).into())
                    }
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
            SoundFunctorType::Wave{n_waves, waves, ..} => yew::html!{<div id="inputs" style="grid-template-columns:repeat(4,1fr)">
                {for waves.iter().enumerate().map(|(i, wave)| yew::html!{<>
                    <div id="wave-options">
                        <input::Button
                        key={wave.id() * 5 + 2}
                        id={i * 5 + 2}
                        desc={"Remove wave element"}
                        component_id={self.id}>
                            <div>{"Remove"}</div>
                        </input::Button>
                        <input::Button
                        key={wave.id() * 5 + 3}
                        id={i * 5 + 3}
                        desc={"Toggle tone input mode"}
                        component_id={self.id}>
                            <div>
                                {if let Tone::Freq(_) = wave.tone() {"Frequency"} else {"Pitch"}}
                            </div>
                        </input::Button>
                    </div>
                    if let Tone::Pitch(pitch) = wave.tone() {
                        <input::Switch
                        key={wave.id() * 5}
                        id={i * 5}
                        options={Wave::NOTE_NAMES.to_vec()}
                        name={"Note"}
                        component_id={self.id}
                        initial={pitch as usize}/>
                    } else if let Tone::Freq(freq) = wave.tone() {
                        <input::Slider
                        key={wave.id() * 5}
                        id={i * 5}
                        coef={Wave::MAX_FREQ} precision={0}
                        postfix={"Hz"}
                        name={"Frequency"}
                        component_id={self.id}
                        initial={freq as f64}/>
                    }
                    <input::Switch
                    key={wave.id() * 5 + 1}
                    id={i * 5 + 1}
                    options={Wave::WAVE_TYPE_NAMES.to_vec()}
                    name={"Wave type"}
                    component_id={self.id}
                    initial={wave.wave_type()}/>
                    <input::Slider
                    key={wave.id() * 5 + 4}
                    id={i * 5 + 4}
                    name={"Wave shift"}
                    component_id={self.id}
                    initial={wave.shift()}/>
                </>})}
                <input::Button
                key={n_waves * 5}
                id={waves.len() * 5}
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
                    MainCmd::Select(Some(self.id))]),
                FocusType::Moving(_) => ctx.send_message(MainCmd::Select(Some(self.id))),
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
