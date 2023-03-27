use std::ops::{Not, Deref, DerefMut};
use wasm_bindgen::JsCast;
use web_sys::{
    AudioNode, AnalyserNode,
    AudioContext,
    GainNode, GainOptions,
    OscillatorNode, OscillatorOptions, OscillatorType,
    CanvasRenderingContext2d, Path2d as JsPath2d};
use yew::{html, Html};
use crate::{
    utils::{
        JsResult,
        Point,
        HitZone,
        HorizontalArrow,
        Rhombus,
        JsResultUtils, BoolExt, SliceExt, VecExt},
    input::{Switch, Slider, Button, ParamId},
    MainCmd,
    js_try, loc};

impl WaveDef {
    fn new(id: usize) -> Self {
        Self{id, freq: 440.0, wave_type: Sound::WAVE_TYPES[0]}
    }
}

#[derive(Clone, Debug)]
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
        js_try!{
            match self {
                Self::InputFreq(freq) => {
                    let ctrl = GainNode::new_with_options(ctx, GainOptions::new().gain(f32::MIN_POSITIVE)).add_loc(loc!())?;
                    for wave in waves {
                        let wave = OscillatorNode::new_with_options(ctx, OscillatorOptions::new()
                            .frequency(wave.freq + freq)
                            .type_(wave.wave_type)).add_loc(loc!())?;
                        wave.start().add_loc(loc!())?;
                        wave.connect_with_audio_node(&ctrl).add_loc(loc!())?;
                    }
                    Self::Wave(ctrl)}
                x @Self::End => x,
                x => JsResult::<!>::Err(format!("`{}` sound cannot be turned into a `Complex Wave`", x.name()).into()).add_loc(loc!())?
            }
        }.explain_err("in sound::Sound::wave")
    }

    // `attack`, `decay` and `release` are in microseconds, `sustain` must be in [0; 1]
    pub fn envelope(self, ctx: &AudioContext, attack: u32, decay: u32, sustain: f32, release: u32) -> JsResult<Self> {
        js_try!{
            let ctrl = match self {
                Self::InputFreq(freq) => {
                    let ctrl = GainNode::new_with_options(ctx, GainOptions::new().gain(f32::MIN_POSITIVE)).add_loc(loc!())?;
                    OscillatorNode::new_with_options(ctx, OscillatorOptions::new().frequency(freq)).add_loc(loc!())?
                        .connect_with_audio_node(&ctrl).add_loc(loc!())?;
                    ctrl}
                Self::Wave(x) => x,
                Self::Envelope{ctrl, ..} => ctrl,
                x @Self::End => return Ok(x)};
            Self::Envelope{attack: Some(attack), decay, sustain, release, ctrl}
        }.explain_err("in sound::Sound::envelope")
    }

    #[inline] pub fn name(&self) -> &'static str {
        match self {
            Self::InputFreq(_) => "Input Frequency",
            Self::Wave(_) => "Complex Wave",
            Self::Envelope{..} => "Envelope",
            Self::End => "Ending Signal"}
    }

    pub fn prepare(self, dest: &AudioNode) -> JsResult<Self> {
        js_try!{
            match self {
                Self::InputFreq(freq) => {
                    let ctx = dest.context();
                    let ctrl = GainNode::new_with_options(&ctx, GainOptions::new().gain(f32::MIN_POSITIVE)).add_loc(loc!())?;
                    OscillatorNode::new_with_options(&ctx, OscillatorOptions::new().frequency(freq)).add_loc(loc!())?
                        .connect_with_audio_node(&ctrl).add_loc(loc!())?
                        .connect_with_audio_node(dest).add_loc(loc!())?;
                    Self::Wave(ctrl)}

                Self::Wave(ctrl) => {
                    ctrl.connect_with_audio_node(dest).add_loc(loc!())?;
                    Self::Wave(ctrl)}

                Self::Envelope{attack, decay, sustain, release, ctrl} => {
                    ctrl.connect_with_audio_node(dest).add_loc(loc!())?;
                    Self::Envelope{attack, decay, sustain, release, ctrl}}

                x => JsResult::<!>::Err(format!("`{}` sound cannot be played", x.name()).into()).add_loc(loc!())?
            }
        }.explain_err("in sound::Sound::prepare")
    }

    pub fn progress(&mut self) -> JsResult<Option<u32>> {
        js_try!{
            match self {
                Self::Wave(ctrl) => {
                    let ctx: AudioContext = ctrl.context().unchecked_into();
                    ctrl.gain().set_value_at_time(1.0, ctx.current_time()).add_loc(loc!())?;
                    None}
                Self::Envelope{attack, decay, sustain, ctrl, ..} => {
                    let ctx: AudioContext = ctrl.context().unchecked_into();
                    if let Some(attack) = attack.take() {
                        ctrl.gain().linear_ramp_to_value_at_time(1.0, ctx.current_time() + attack as f64 / 1000.0).add_loc(loc!())?;
                        Some(attack)
                    } else {
                        ctrl.gain().linear_ramp_to_value_at_time(*sustain, ctx.current_time() + *decay as f64 / 1000.0).add_loc(loc!())?;
                        None}
                }
                x => JsResult::<!>::Err(format!("`{}` sound cannot be played", x.name()).into()).add_loc(loc!())?
            }
        }.explain_err("in sound::Sound::progress")
    }

    // the returned float signifies when the object can be discarded
    pub fn end(&mut self) -> JsResult<u32> {
        js_try!{
            match self {
                Self::Wave(ctrl) => {
                    let ctx: AudioContext = ctrl.context().unchecked_into();
                    ctrl.gain().set_value_at_time(f32::MIN_POSITIVE, ctx.current_time()).add_loc(loc!())?;
                    0}
                Self::Envelope{release, ctrl, ..} => {
                    let ctx: AudioContext = ctrl.context().unchecked_into();
                    ctrl.gain().cancel_scheduled_values(0.0).add_loc(loc!())?
                        .linear_ramp_to_value_at_time(f32::MIN_POSITIVE, ctx.current_time() + *release as f64 / 1000.0).add_loc(loc!())?;
                    *release}

                x => JsResult::<!>::Err(format!("`{}` sound cannot be played", x.name()).into()).add_loc(loc!())?
            }
        }.explain_err("in sound::Sound::end")
    }

    pub fn disconnect(self) -> JsResult<()> {
        match self {
            Self::Wave(ctrl) | Self::Envelope{ctrl, ..}
                => ctrl.disconnect(),
            _ => Ok(())
        }.explain_err("in sound::Sound::disconnect")
    }
}

#[derive(Debug)]
enum Focus {
    None,
    Moving(Point),
    Connecting(Point),
}

#[derive(Debug)]
pub struct Element {
    location: Point,
    focus: Focus,
    id: usize
}

/// used by the implementation of `SoundGen::Wave`
#[derive(Debug)]
pub struct WaveDef{id: usize, freq: f32, wave_type: OscillatorType}

/// represents a sound transformer that can have an optional input element
/// and an optional output element
#[derive(Debug)]
pub enum SoundGen {
    /// emits a stub sound when it's time to start playing, doesn't accept any input
    Input{base: Element},
    /// generates a wave combined from a variable number of primitive waves of customizable form
    Wave{base: Element, waves: Vec<WaveDef>, n_waves: usize},
    /// wraps the input sound in an "envelope": https://en.wikipedia.org/wiki/Envelope_(music)
    Envelope{base: Element, attack: u32, decay: u32, sustain: f32, release: u32},
    /// consumes the input sound, delegating it to the `SoundPlayer`
    Output{base: Element},
}

impl Deref for SoundGen {
    type Target = Element;
    fn deref(&self) -> &Self::Target {
        match self {
            SoundGen::Input{base} => base,
            SoundGen::Wave {base, ..} => base,
            SoundGen::Envelope{base, ..} => base,
            SoundGen::Output{base} => base}
    }
}

impl DerefMut for SoundGen {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            SoundGen::Input{base} => base,
            SoundGen::Wave {base, ..} => base,
            SoundGen::Envelope{base, ..} => base,
            SoundGen::Output{base} => base}
    }
}

/// used to visualise certain sound elements while editing them
pub struct Graph {
    pub path: JsPath2d,
    pub span: u32,
    pub in_span: u32
}

impl SoundGen {
    const VISUAL_SIZE: i32 = 32;
    #[inline] pub fn new_input(location: Point) -> Self {
        Self::Input{base: Element{location, focus: Focus::None, id: 0}}
    }

    #[inline] pub fn new_wave(location: Point) -> Self {
        Self::Wave{waves: vec![], n_waves: 0,
            base: Element{location, focus: Focus::None, id: 0}}
    }

    #[inline] pub fn new_envelope(location: Point) -> Self {
        Self::Envelope{attack: 0, decay: 0, sustain: 0.0, release: 0,
            base: Element{location, focus: Focus::None, id: 0}}
    }

    #[inline] pub fn new_output(location: Point) -> Self {
        Self::Output{base: Element{location, focus: Focus::None, id: 0}}
    }

    #[inline] pub fn id(&self) -> usize {self.id}
    #[inline] pub fn set_id(&mut self, id: usize) {self.id = id}

    #[inline] pub fn name(&self) -> &'static str {
        match &self {
            Self::Input{..} => "User Input",
            Self::Wave{..} => "Wave generator",
            Self::Envelope{..} => "Envelope",
            Self::Output{..} => "Output"}
    }

    #[inline] pub fn graphable(&self) -> bool {
        matches!(self, Self::Envelope{..})
    }

    /// `self` is the source, `other` is the destination
    pub fn connectible(&self, other: &Self) -> bool {
        if self.id == other.id || matches!(self, SoundGen::Output{..}) || matches!(other, SoundGen::Input{..}) {
            return false
        }
        true
    }

    /// the outward connections of the component will visually start from the returned point
    #[inline] pub fn output_point(&self) -> Option<Point> {
        matches!(self, Self::Output{..}).not()
            .then(|| self.location + Point{x: Self::VISUAL_SIZE, y: 0})
    }

    /// the inward connections of the component will visually end at the returned point
    #[inline] pub fn input_point(&self) -> Option<Point> {
        matches!(self, Self::Input{..}).not()
            .then(|| self.location - Point{x: Self::VISUAL_SIZE, y: 0})
    }

    /// the returned optional integer marks the interval (in milliseconds) after which
    /// this function should be called again with a stub sound as input
    pub fn transform(&mut self, player: &mut SoundPlayer, sound: Sound) -> JsResult<(Sound, Option<u32>)> {
        let ctx = player.audio_context();
        match self {
            Self::Wave{waves, ..} =>
                sound.wave(ctx, waves.iter()).map(|x| (x, None)),
            Self::Envelope{attack, decay, sustain, release, ..} =>
                sound.envelope(ctx, *attack, *decay, *sustain, *release).map(|x| (x, None)),
            Self::Output{..} =>
                player.play_sound(sound).map(|_| (Sound::End, None)),
            Self::Input{..} => Ok((sound, None))
        }.explain_err("in sound::SoundGen::transform")
    }

    #[inline] pub fn get_conn_creation_hitzone(&self) -> impl HitZone {
        HorizontalArrow::new(self.location, Self::VISUAL_SIZE / 2, Self::VISUAL_SIZE / 2, false)
            .shift_x(Self::VISUAL_SIZE / 2)
    }

    pub fn contains(&self, point: Point) -> bool {
        match (self.output_point().is_some(), self.input_point().is_some()) {
            (true,  true) => Rhombus::new(self.location, Self::VISUAL_SIZE, Self::VISUAL_SIZE)
                .contains(point),
            (_, x) => HorizontalArrow::new(self.location, Self::VISUAL_SIZE, Self::VISUAL_SIZE, x)
                .contains(point) // the values may never be `(false, false)`
        }
    }

    pub fn handle_movement(&mut self, coords: Point, is_last: bool) -> Option<MainCmd> {
        let (focus, cmd) = match self.focus {
            Focus::None =>
                if self.output_point().is_some() && self.get_conn_creation_hitzone().contains(coords) {
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

    pub fn params(&self) -> Html {
        match &self {
            Self::Wave{waves, n_waves, ..} => html!{<div id="inputs" style="grid-template-columns:repeat(4,1fr)">
                {for waves.iter().enumerate().map(|(i, wave)| html!{<>
                    <div id="wave-options">
                        <Button
                        key={wave.id * 5 + 2}
                        id={ParamId::RemoveWave(self.id, i as u32)}
                        desc={"Remove wave element"}>
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
                    id={ParamId::WaveFreq(self.id, i as u32)}
                    coef={Sound::MAX_WAVE_FREQ} precision={0}
                    postfix={"Hz"}
                    name={"Frequency"}
                    initial={wave.freq as f64}/>
                    <Switch
                    key={wave.id * 5 + 1}
                    id={ParamId::WaveType(self.id, i as u32)}
                    options={Sound::WAVE_TYPE_NAMES.to_vec()}
                    name={"Wave type"}
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
                id={ParamId::AddWave(self.id)}
                desc={"Add new wave element"}
                style={"grid-column: 1 / -1; width: auto"}>
                    <svg viewBox="0 0 100 100" style="height:100%">
                        <polygon points="45,25 55,25 55,45 75,45 75,55 55,55 55,75 45,75 45,55 25,55 25,45 45,45"/>
                    </svg>
                </Button>
            </div>},

            Self::Envelope{attack, decay, sustain, release, ..} => html! {<div id="inputs">
                <Slider name={"Attack time"}
                    id={ParamId::EnvelopeAttack(self.id)}
                    coef={Sound::MAX_INTERVAL}
                    postfix={"ms"} precision={0}
                    initial={*attack as f64}/>
                <Slider name={"Decay time"}
                    id={ParamId::EnvelopeDecay(self.id)}
                    coef={Sound::MAX_INTERVAL}
                    postfix={"ms"} precision={0}
                    initial={*decay as f64}/>
                <Slider name={"Sustain level"}
                    id={ParamId::EnvelopSustain(self.id)}
                    coef={1.0}
                    postfix={""} precision={2}
                    initial={*sustain as f64}/>
                <Slider name={"Release time"}
                    id={ParamId::EnvelopeRelease(self.id)}
                    coef={Sound::MAX_INTERVAL}
                    postfix={"ms"} precision={0}
                    initial={*release as f64}/>
            </div>},
            _ => Default::default()}
    }

    /// the returned boolean marks whether the sound element's editor window should be rerendered
    pub fn set_param(&mut self, id: ParamId, value: f64) -> JsResult<bool> {
        js_try!{
            match self {
                Self::Wave{waves, n_waves, ..} => match id {
                    ParamId::WaveFreq(_, id) => {
                        waves.get_mut_or_js_error(id as usize, "wave element #", " not found").add_loc(loc!())?
                            .freq = value as f32;
                        false}
                    ParamId::WaveType(_, id) => {
                        waves.get_mut_or_js_error(id as usize, "wave element #", " not found").add_loc(loc!())?
                            .wave_type = *Sound::WAVE_TYPES
                                .get_or_js_error(value as usize, "wave type #", " not found").add_loc(loc!())?;
                        false}
                    ParamId::RemoveWave(_, id) =>
                        value.is_sign_negative().then(|| waves.remove(id as usize)).is_some(),
                    ParamId::AddWave(_) =>
                        value.is_sign_negative().then(|| {
                            waves.push(WaveDef::new(*n_waves));
                            *n_waves += 1;
                        }).is_some(),
                    id => JsResult::<!>::Err(format!("`{}` sound element has no parameter `{:?}`", self.name(), id).into()).add_loc(loc!())?
                }

                Self::Envelope{attack, decay, sustain, release, ..} => match id {
                    ParamId::EnvelopeAttack(_) =>  {*attack  = value as u32; false}
                    ParamId::EnvelopeDecay(_)  =>  {*decay   = value as u32; false}
                    ParamId::EnvelopSustain(_) =>  {*sustain = value as f32; false}
                    ParamId::EnvelopeRelease(_) => {*release = value as u32; false}
                    id => JsResult::<!>::Err(format!("`{}` sound element has no parameter `{:?}`", self.name(), id).into()).add_loc(loc!())?
                }

                x => JsResult::<!>::Err(format!("cannot set a parameter on a `{}` sound element", x.name()).into()).add_loc(loc!())?
            }
        }.explain_err("in sound::SoundGen::set_param")
    }

    pub fn draw(&self, ctx: &CanvasRenderingContext2d, mut offset: Point) {
        offset = -offset;
        if self.output_point().is_some() {
            if self.input_point().is_some() {
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

    pub fn graph(&self, width: f64, height: f64) -> JsResult<Graph> {
        let path = JsPath2d::new().explain_err("in sound::Sound::graph").add_loc(loc!())?;
        match self {
            &Self::Envelope {attack, decay, sustain, release, ..} => {
                let in_span = attack + decay;
                let span = in_span + release;
                path.move_to(0.0, height);
                path.line_to(attack as f64 / span as f64 * width, 0.0);
                path.line_to(in_span as f64 / span as f64 * width, (1.0 - sustain) as f64 * height);
                path.line_to(width, height);
                Ok(Graph{path, span, in_span})}

            _ => Ok(Graph{path, in_span: 0, span: 0})
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum SoundState {
    Active(f64),
    Ending(f64),
}

impl Deref for SoundState {
    type Target = f64;
    fn deref(&self) -> &Self::Target {
        match self {
            Self::Active(x) => x,
            Self::Ending(x) => x}
    }
}

pub struct SoundPlayer {
    sounds: Vec<(Sound, SoundState)>,
    ending_all: bool,
    output: AnalyserNode,
    audio_context: AudioContext
}

impl SoundPlayer {
    pub fn new() -> JsResult<Self> {
        js_try!{
            let audio_context = AudioContext::new().add_loc(loc!())?;
            let output = AnalyserNode::new(&audio_context).add_loc(loc!())?;
            output.connect_with_audio_node(&audio_context.destination()).add_loc(loc!())?;
            Self{sounds: vec![], ending_all: false, output, audio_context}
        }.explain_err("in sound::SoundPlayer::new")
    }

    pub fn play_sound(&mut self, sound: Sound) -> JsResult<()> {
        js_try!{
            if matches!(sound, Sound::End) {
                self.ending_all = true;
            } else {
                self.sounds.push((sound.prepare(&self.output).add_loc(loc!())?, SoundState::Active(0.0)));
            }
        }.explain_err("in sound::Sound::play_sound")
    }

    #[inline] pub fn audio_context(&self) -> &AudioContext {
        &self.audio_context
    }

    #[inline] pub fn output(&self) -> &AnalyserNode {
        &self.output
    }

    pub fn poll(&mut self, time: f64) -> JsResult<()> {
        js_try!{
            if self.ending_all {
                self.ending_all = false;
                let mut err = Ok(()); // making sure to drop as many sounds as possible
                for (sound, state) in self.sounds.iter_mut() {
                    let after = match sound.end().add_loc(loc!()) {
                        Ok(after) => after,
                        Err(x) => {err = err.and(Err(x)); 0}};
                    *state = SoundState::Ending(after as f64 + time);
                }
                self.sounds.sort_unstable_by(|(_, s1), (_, s2)| s1.total_cmp(s2));
                err?;
            }

            let len = self.sounds.len();
            let start = match self.sounds.iter().rev().position(|&(_, state)| time < *state) {
                Some(x) => (x > 0).then(|| len - x),
                None => Some(0)};
            let mut err = Ok(());
            if let Some(start) = start {
                let pending: Vec<_> = self.sounds.drain(start..).collect();
                for (mut sound, mut state) in pending {
                    match state {
                        SoundState::Active(_) => {
                            state = match sound.progress().add_loc(loc!())? {
                                Some(after) => SoundState::Active(after as f64 + time),
                                None => SoundState::Active(f64::INFINITY)};
                            self.sounds.push_sorted((sound, state),
                                |x, y| x.1.total_cmp(&y.1).reverse());
                        }
                        // not using the `?` operator to remove as much sounds as possible
                        SoundState::Ending(_) => err = err.and(sound.disconnect().add_loc(loc!()))
                    }
                }
            }
            err?}
    }
}
