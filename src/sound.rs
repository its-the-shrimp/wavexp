use std::{
    rc::{Rc, Weak},
    sync::Mutex, ops::Not};
use gloo_timers::callback::Timeout;
use wasm_bindgen::JsCast;
use web_sys::{
    AudioNode,
    AudioContext,
    GainNode, GainOptions,
    OscillatorNode, OscillatorOptions, OscillatorType,
    AnalyserNode,
    CanvasRenderingContext2d, Path2d};
use yew::html;
use crate::{
    utils::{
        JsResult,
        Point,
        HitZone,
        HorizontalArrow,
        Rhombus,
        JsResultUtils, BoolExt, ResultToJsResult, OptionToJsResult, SliceExt},
    input::{Switch, Slider, Button},
    MainCmd,
    js_try};

pub struct WaveDef{id: usize, freq: f32, wave_type: OscillatorType}

impl WaveDef {
    fn new(id: usize) -> Self {
        Self{id, freq: 440.0, wave_type: Sound::WAVE_TYPES[0]}
    }
}

#[derive(Clone)]
pub enum Sound {
    InputFreq(f32),
    Wave(GainNode),
    Envelope{attack: Option<u32>, decay: u32, sustain: f32, release: u32, ctrl: GainNode},
    End
}

impl Sound {
    const WAVE_TYPES: [OscillatorType; 4] = [
        OscillatorType::Sine,
        OscillatorType::Square,
        OscillatorType::Sawtooth,
        OscillatorType::Triangle];
    const WAVE_TYPE_NAMES: [&'static str; 4] = [
        "Sine",
        "Square",
        "Saw",
        "Triangle"];
    const MAX_WAVE_FREQ: f64 = 5000.0;
    const MAX_INTERVAL: f64 = 2000.0;

    pub fn wave<'a>(self, ctx: &AudioContext, waves: impl Iterator<Item = &'a WaveDef>) -> JsResult<Self> {
        match self {
            Self::InputFreq(freq) => {
                let ctrl = GainNode::new_with_options(ctx, GainOptions::new().gain(f32::MIN_POSITIVE))?;
                for wave in waves {
                    let wave = OscillatorNode::new_with_options(ctx, OscillatorOptions::new()
                        .frequency(wave.freq + freq)
                        .type_(wave.wave_type))?;
                    wave.start()?;
                    wave.connect_with_audio_node(&ctrl)?;
                }
                Ok(Self::Wave(ctrl))}
            x @Self::End => Ok(x),
            x => Err(format!("`{}` sound cannot be turned into a `Complex Wave`", x.name()).into())
        }
    }

    // `attack`, `decay` and `release` are in microseconds, `sustain` must be in [0; 1]
    pub fn envelope(self, ctx: &AudioContext, attack: u32, decay: u32, sustain: f32, release: u32) -> JsResult<Self> {
        let ctrl = match self {
            Self::InputFreq(freq) => {
                let ctrl = GainNode::new_with_options(ctx, GainOptions::new().gain(f32::MIN_POSITIVE))?;
                OscillatorNode::new_with_options(ctx, OscillatorOptions::new().frequency(freq))?
                    .connect_with_audio_node(&ctrl)?;
                ctrl}
            Self::Wave(x) => x,
            Self::Envelope{ctrl, ..} => ctrl,
            x @Self::End => return Ok(x)};
        Ok(Self::Envelope{attack: Some(attack), decay, sustain, release, ctrl})
    }

    #[inline] pub fn name(&self) -> &'static str {
        match self {
            Self::InputFreq(_) => "Input Frequency",
            Self::Wave(_) => "Complex Wave",
            Self::Envelope{..} => "Envelope",
            Self::End => "Ending Signal"}
    }

    pub fn prepare(self, dest: &AudioNode) -> JsResult<Self> {
        match self {
            Self::InputFreq(freq) => {
                let ctx = dest.context();
                let ctrl = GainNode::new_with_options(&ctx, GainOptions::new().gain(f32::MIN_POSITIVE))?;
                OscillatorNode::new_with_options(&ctx, OscillatorOptions::new().frequency(freq))?
                    .connect_with_audio_node(&ctrl)?
                    .connect_with_audio_node(dest)?;
                Ok(Self::Wave(ctrl))}

            Self::Wave(ctrl) => {
                ctrl.connect_with_audio_node(dest)?;
                Ok(Self::Wave(ctrl))}

            Self::Envelope{attack, decay, sustain, release, ctrl} => {
                ctrl.connect_with_audio_node(dest)?;
                Ok(Self::Envelope{attack, decay, sustain, release, ctrl})}

            x => Err(format!("`{}` sound cannot be played", x.name()).into())
        }
    }

    fn progress(&mut self) -> JsResult<Option<u32>> {
        js_try!{
            match self {
                Self::Wave(ctrl) => {
                    let ctx: AudioContext = ctrl.context().unchecked_into();
                    ctrl.gain().set_value_at_time(1.0, ctx.current_time())?;
                    None}
                Self::Envelope{attack, decay, sustain, ctrl, ..} => {
                    let ctx: AudioContext = ctrl.context().unchecked_into();
                    if let Some(attack) = attack.take() {
                        ctrl.gain().linear_ramp_to_value_at_time(1.0, ctx.current_time() + attack as f64 / 1000.0)?;
                        Some(attack)
                    } else {
                        ctrl.gain().linear_ramp_to_value_at_time(*sustain, ctx.current_time() + *decay as f64 / 1000.0)?;
                        None}
                }
                x => JsResult::<!>::Err(format!("`{}` sound cannot be played", x.name()).into())?
            }
        }.explain_err("polling the sound as a future")
    }

    // the returned float signifies when the object can be discarded
    pub fn end(self) -> JsResult<u32> {
        match self {
            Self::Wave(ctrl) => {
                let ctx: AudioContext = ctrl.context().unchecked_into();
                ctrl.gain().set_value_at_time(f32::MIN_POSITIVE, ctx.current_time())?;
                Ok(0)
            }
            Self::Envelope{release, ctrl, ..} => {
                let ctx: AudioContext = ctrl.context().unchecked_into();
                ctrl.gain().cancel_scheduled_values(0.0)?
                    .linear_ramp_to_value_at_time(f32::MIN_POSITIVE, ctx.current_time() + release as f64 / 1000.0)?;
                Ok(release)
            }
            x => Err(format!("`{}` sound cannot be played", x.name()).into())
        }
    }
}

#[derive(Debug)]
enum Focus {
    None,
    Moving(Point),
    Connecting(Point),
}

enum SoundGenType {
    Input,
    Wave{waves: Vec<WaveDef>, n_waves: usize},
    Envelope{attack: u32, decay: u32, sustain: f32, release: u32},
    Output{analyser: Rc<AnalyserNode>, sounds: Vec<Rc<Mutex<(Sound, Option<Timeout>)>>>}
    // this is absolutely ugly
}

impl SoundGenType {
    #[inline] pub fn name(&self) -> &'static str {
        match &self {
            SoundGenType::Input => "User Input",
            SoundGenType::Wave{..} => "Wave generator",
            SoundGenType::Envelope{..} => "Envelope",
            SoundGenType::Output{..} => "Output"}
    }
}

pub struct SoundGen {
    gen_type: SoundGenType,
    location: Point,
    focus: Focus,
    pub id: usize
}

impl SoundGen {
    const VISUAL_SIZE: i32 = 32;
    #[inline] pub fn new_input(id: usize, location: Point) -> Self {
        Self{gen_type: SoundGenType::Input,
            location, focus: Focus::None, id}
    }

    #[inline] pub fn new_wave(id: usize, location: Point) -> Self {
        Self{gen_type: SoundGenType::Wave{waves: vec![], n_waves: 0},
            location, focus: Focus::None, id}
    }

    #[inline] pub fn new_envelope(id: usize, location: Point) -> Self {
        Self{gen_type: SoundGenType::Envelope{attack: 0, decay: 0, sustain: 0.0, release: 0},
            location, focus: Focus::None, id}
    }

    #[inline] pub fn new_output(id: usize, location: Point, analyser: Rc<AnalyserNode>) -> Self {
        Self{gen_type: SoundGenType::Output{analyser, sounds: vec![]},
            location, focus: Focus::None, id}
    }

    #[inline] pub fn name(&self) -> &'static str {self.gen_type.name()}

    // the outward connections of the component will visually start from the returned point
    #[inline] pub fn forwardable(&self) -> Option<Point> {
        matches!(self.gen_type, SoundGenType::Output{..}).not()
            .then(|| self.location + Point{x: Self::VISUAL_SIZE, y: 0})
    }

    // the inward connections of the component will visually end at the returned point
    #[inline] pub fn backwardable(&self) -> Option<Point> {
        matches!(self.gen_type, SoundGenType::Input{..}).not()
            .then(|| self.location - Point{x: Self::VISUAL_SIZE, y: 0})
    }

    #[inline] fn get_conn_creation_hitzone(&self) -> impl HitZone {
        HorizontalArrow::new(self.location, Self::VISUAL_SIZE / 2, Self::VISUAL_SIZE / 2, false)
            .shift_x(Self::VISUAL_SIZE / 2)
    }

    pub fn contains(&self, point: Point) -> bool {
        match (self.forwardable().is_some(), self.backwardable().is_some()) {
            (true,  true) => Rhombus::new(self.location, Self::VISUAL_SIZE, Self::VISUAL_SIZE)
                .contains(point),
            (_, x) => HorizontalArrow::new(self.location, Self::VISUAL_SIZE, Self::VISUAL_SIZE, x)
                .contains(point) // the values may never be `(false, false)`
        }
    }

    pub fn handle_movement(&mut self, coords: Point, is_last: bool) -> Option<MainCmd> {
        let (focus, cmd) = match self.focus {
            Focus::None =>
                if self.forwardable().is_some() && self.get_conn_creation_hitzone().contains(coords) {
                    (Focus::Connecting(coords),
                        Some(MainCmd::SetDesc(format!("{}: Connecting", self.name()))))
                } else {
                    (Focus::Moving(self.location - coords),
                        Some(MainCmd::SetDesc(format!("{}: Moving", self.name()))))
                }
            Focus::Moving(offset) => {
                self.location = coords + offset;
                is_last.choose(
                    (Focus::None, Some(MainCmd::Select(Some(self.id)))),
                    (Focus::Moving(offset), None))
            }
            Focus::Connecting(dst) => is_last.choose(
                (Focus::None, Some(MainCmd::TryConnect(self.id, dst))),
                (Focus::Connecting(coords), None))
        };
        self.focus = focus;
        cmd
    }

    pub fn transform(&mut self, ctx: &AudioContext, sound: Sound) -> JsResult<Sound> {
        match &mut self.gen_type {
            SoundGenType::Wave{waves, ..} => sound.wave(ctx, waves.iter()),
            SoundGenType::Envelope{attack, decay, sustain, release} =>
                sound.envelope(ctx, *attack, *decay, *sustain, *release),
            SoundGenType::Output{analyser, sounds} => {
                fn poll_sound(sound: Weak<Mutex<(Sound, Option<Timeout>)>>) {
                    _ = js_try!{
                        let Some(sound_owned) = sound.upgrade() else {return};
                        let mut sound_owned = sound_owned.try_lock().to_js_result()?;
                        let Some(after) = sound_owned.0.progress()? else {return};
                        sound_owned.1 = Some(Timeout::new(after, move || poll_sound(sound)));
                    }.report_err("polling the sound");
                }

                if let Sound::End = sound {
                    for sound in sounds.drain(..) {
                        Rc::try_unwrap(sound).ok()
                            .to_js_result("failed to get unique reference to a sound while ending it")?
                            .into_inner().to_js_result()?.0.end()?;
                    }
                } else {
                    let new = Rc::new(Mutex::new((sound.prepare(&analyser)?, None)));
                    poll_sound(Rc::downgrade(&new));
                    sounds.push(new);
                }
                Ok(Sound::End)}
            SoundGenType::Input => Err("`User Input` sound element cannot transform sound".into())
        }
    }

    pub fn params(&self) -> yew::Html {
        match &self.gen_type {
            SoundGenType::Wave{waves, n_waves} => html!{<div id="inputs" style="grid-template-columns:repeat(4,1fr)">
                {for waves.iter().enumerate().map(|(i, wave)| html!{<>
                    <div id="wave-options">
                        <Button
                        key={wave.id * 5 + 2}
                        id={i * 5 + 2}
                        desc={"Remove wave element"}
                        component_id={self.id}>
                            <div>{"Remove"}</div>
                        </Button>
                        /*<Button
                        key={wave.id * 5 + 3}
                        id={i * 5 + 3}
                        desc={"Toggle tone input mode"}
                        component_id={self.id}>
                            <div>
                                {if let Tone::Freq(_) = wave.tone() {"Frequency"} else {"Pitch"}}
                            </div>
                        </Button>*/
                    </div>
                    /*if let Tone::Pitch(pitch) = wave.tone() {
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
                    }*/
                    <Slider
                    key={wave.id * 5}
                    id={i * 5}
                    coef={Sound::MAX_WAVE_FREQ} precision={0}
                    postfix={"Hz"}
                    name={"Frequency"}
                    component_id={self.id}
                    initial={wave.freq as f64}/>
                    <Switch
                    key={wave.id * 5 + 1}
                    id={i * 5 + 1}
                    options={Sound::WAVE_TYPE_NAMES.to_vec()}
                    name={"Wave type"}
                    component_id={self.id}
                    initial={Sound::WAVE_TYPES.iter().position(|&x| x == wave.wave_type).unwrap_or(0)}/>
                    /*<Slider
                    key={wave.id * 5 + 4}
                    id={i * 5 + 4}
                    name={"Wave shift"}
                    component_id={self.id}
                    initial={wave.shift}/>*/
                </>})}
                <Button
                key={n_waves * 5}
                id={waves.len() * 5}
                desc={"Add new wave element"}
                component_id={self.id}
                style={"grid-column: 1 / -1; width: auto"}>
                    <svg viewBox="0 0 100 100" style="height:100%">
                        <polygon points="45,25 55,25 55,45 75,45 75,55 55,55 55,75 45,75 45,55 25,55 25,45 45,45"/>
                    </svg>
                </Button>
            </div>},

            SoundGenType::Envelope{attack, decay, sustain, release, ..} => html! {<div id="inputs">
                <Slider
                    id={0}
                    coef={Sound::MAX_INTERVAL}
                    postfix={"ms"} precision={0}
                    name={"Attack time"}
                    component_id={self.id}
                    initial={*attack as f64}/>
                <Slider
                    id={1}
                    coef={Sound::MAX_INTERVAL}
                    postfix={"ms"} precision={0}
                    name={"Decay time"}
                    component_id={self.id}
                    initial={*decay as f64}/>
                <Slider
                    id={2}
                    coef={1.0}
                    postfix={""}
                    name={"Sustain level"}
                    component_id={self.id}
                    initial={*sustain as f64}/>
                <Slider
                    id={3}
                    coef={Sound::MAX_INTERVAL}
                    postfix={"ms"} precision={0}
                    name={"Release time"}
                    component_id={self.id}
                    initial={*release as f64}/>
            </div>},
            _ => Default::default()}
    }

    pub fn set_param(&mut self, id: usize, value: f64) -> JsResult<bool> {
        match &mut self.gen_type {
            SoundGenType::Wave{waves, n_waves} => {
                let (wave_id, param_id) = (id / 5, id % 5);
                if wave_id == waves.len() {
                    Ok(value.is_sign_negative().then(|| {
                        waves.push(WaveDef::new(*n_waves));
                        *n_waves += 1;
                    }).is_some())
                } else {
                    let wave = waves.get_mut_or_js_error(wave_id, "wave component #", " not found")?;
                    match param_id {
                        0 => {
                            wave.freq = value as f32;
                            Ok(false)}
                        1 => {
                            wave.wave_type = *Sound::WAVE_TYPES
                                .get_or_js_error(value as usize, "wave type #", " not found")?;
                            Ok(false)}
                        2 => Ok(value.is_sign_negative().then(||
                            waves.remove(wave_id)).is_some()),
                        3 => Ok(false), // TODO: tone input mode
                        4 => Ok(false), // TODO: shift
                        x => Err(format!("invalid parameter ID #{} of a `Wave` sound element", x).into())
                    }
                }
            }

            SoundGenType::Envelope{attack, decay, sustain, release, ..} => match id {
                0 => {*attack  = value as u32; Ok(false)}
                1 => {*decay   = value as u32; Ok(false)}
                2 => {*sustain = value as f32; Ok(false)}
                3 => {*release = value as u32; Ok(false)}
                x => Err(format!("invalid parameter ID {} of an `Envelope` sound element", x).into())}

            x => Err(format!("cannot set a parameter on a `{}` sound element", x.name()).into())
        }
    }

    pub fn draw(&self, ctx: &CanvasRenderingContext2d, mut offset: Point) {
        offset = -offset;
        if self.forwardable().is_some() {
            if self.backwardable().is_some() {
                Rhombus::new(self.location, Self::VISUAL_SIZE, Self::VISUAL_SIZE)
                    .shift(offset).draw(ctx);
            } else {
                HorizontalArrow::new(self.location, Self::VISUAL_SIZE, Self::VISUAL_SIZE, false)
                    .shift(offset).draw(ctx);
            }
            let hitzone = self.get_conn_creation_hitzone().shift(offset);
            hitzone.draw(ctx);
            if let Focus::Connecting(mut conn) = self.focus {
                conn += offset;
                ctx.move_to(hitzone.right().into(), hitzone.center().y.into());
                ctx.line_to(conn.x.into(), conn.y.into());
            }
        } else {
            HorizontalArrow::new(self.location, Self::VISUAL_SIZE, Self::VISUAL_SIZE, true)
                .shift(offset).draw(ctx);
        }
    }

    pub fn graph(&self, width: f64, height: f64) -> JsResult<(Path2d, u32, u32)> {
        match &self.gen_type {
            SoundGenType::Envelope {attack, decay, sustain, release, ..} => {
                let res = (Path2d::new()?, attack + decay);
                let res = (res.0, res.1, res.1 + release);
                res.0.move_to(0.0, height);
                res.0.line_to(*attack as f64 / res.2 as f64 * width, 0.0);
                res.0.line_to(res.1 as f64 / res.2 as f64 * width, (1.0 - sustain) as f64 * height);
                res.0.line_to(width, height);
                Ok(res)}
            _ => Ok((Path2d::new()?, 0, 0))
        }
    }

    pub fn graphable(&self) -> bool {
        matches!(self.gen_type, SoundGenType::Envelope{..})
    }
}
