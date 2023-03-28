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
        JsResultUtils, BoolExt, SliceExt, VecExt, OptionToJsResult, js_error},
    input::{Switch, Slider, Button, ParamId},
    MainCmd,
    loc};

#[derive(Debug, Clone, Copy)]
pub struct Note(u8);

impl Note {
    pub const FREQS: [f32; 24] = [
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

    pub const NAMES: [&'static str; 24] = [
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

    pub const fn index(value: usize) -> Option<Self> {
        if value >= Self::FREQS.len() {return None}
        Some(Self(value as u8))
    }

    /// SAFETY: `value` must be finite
    pub unsafe fn freq_unchecked(value: f32) -> Self {
        const MAX: usize = Note::FREQS.len() - 1;
        Self(Self::FREQS.iter()
            .position(|&freq| value <= freq)
            .unwrap_or(MAX) as u8)
    }

    /*pub fn freq(value: f32) -> Option<Self> {
        if !value.is_finite() {return None}
        Some(unsafe{Self::freq_unchecked(value)})
    }*/

    pub const fn to_index(self) -> usize {
        self.0 as usize
    }

    pub const fn to_freq(self) -> f32 {
        unsafe{*Self::FREQS.get_unchecked(self.0 as usize)}
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Tone {
    Freq(f32),
    Note(Note)
}

impl Tone {
    pub fn to_freq(&self) -> f32 {
        match self {
            Self::Freq(x) => *x,
            Self::Note(x) => x.to_freq()}
    }

    /*pub fn to_note(&self) -> Note {
        match self {
            &Self::Note(x) => x,
            &Self::Freq(x) => unsafe{Note::freq_unchecked(x)}
        }
    }*/

    pub fn toggled(self) -> Self {
        unsafe {
            match self {
                Self::Freq(freq) => Self::Note(Note::freq_unchecked(freq)),
                Self::Note(note) => Self::Freq(note.to_freq())}}
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

    pub fn wave<'a>(self, ctx: &AudioContext, waves: impl Iterator<Item = WaveDef>) -> JsResult<Self> {
        Ok(match self {
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
            x => js_error(format!("`{}` sound cannot be turned into a `Complex Wave`", x.name()), loc!())?
        })
    }

    /// `attack`, `decay` and `release` are in microseconds, `sustain` must be in [0; 1]
    pub fn envelope(self, ctx: &AudioContext, attack: u32, decay: u32, sustain: f32, release: u32) -> JsResult<Self> {
        let ctrl = match self {
            Self::InputFreq(freq) => {
                let ctrl = GainNode::new_with_options(ctx, GainOptions::new().gain(f32::MIN_POSITIVE)).add_loc(loc!())?;
                let gen = OscillatorNode::new_with_options(ctx,
                    OscillatorOptions::new().frequency(WaveDef::DEF_FREQ + freq)).add_loc(loc!())?;
                gen.connect_with_audio_node(&ctrl).add_loc(loc!())?;
                gen.start().add_loc(loc!())?;
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
        Ok(match self {
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

            x => js_error(format!("`{}` sound cannot be played", x.name()), loc!())?
        })
    }

    pub fn progress(&mut self) -> JsResult<Option<u32>> {
        Ok(match self {
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
                    None}}

            x => js_error(format!("`{}` sound cannot be played", x.name()), loc!())?
        })
    }

    /// the returned float signifies when the object can be discarded
    pub fn end(&mut self) -> JsResult<u32> {
        Ok(match self {
            Self::Wave(ctrl) => {
                let ctx: AudioContext = ctrl.context().unchecked_into();
                ctrl.gain().set_value_at_time(f32::MIN_POSITIVE, ctx.current_time()).add_loc(loc!())?;
                0}
            Self::Envelope{release, ctrl, ..} => {
                let ctx: AudioContext = ctrl.context().unchecked_into();
                ctrl.gain().cancel_scheduled_values(0.0).add_loc(loc!())?
                    .linear_ramp_to_value_at_time(f32::MIN_POSITIVE, ctx.current_time() + *release as f64 / 1000.0).add_loc(loc!())?;
                *release}

            x => js_error(format!("`{}` sound cannot be played", x.name()), loc!())?
        })
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
#[derive(Debug, Clone, Copy)]
pub struct WaveDef{
    pub freq: f32,
    pub wave_type: OscillatorType
}

impl Default for WaveDef {
    fn default() -> Self {
        Self{freq: Self::DEF_FREQ, wave_type: OscillatorType::Sine}
    }
}

impl WaveDef {
    const DEF_FREQ: f32 = 440.0;
}

#[derive(Debug, Clone, Copy)]
pub struct RawWaveDef{
    pub id: usize,
    pub tone: Tone,
    pub wave_type: OscillatorType
}

impl From<RawWaveDef> for WaveDef {
    fn from(value: RawWaveDef) -> Self {
        Self{freq: value.tone.to_freq(), wave_type: value.wave_type}
    }
}

impl RawWaveDef {
    fn new(id: usize) -> Self {
        Self{id, tone: Tone::Freq(440.0), wave_type: OscillatorType::Sine}
    }
}

/// represents a sound transformer that can have an optional input element
/// and an optional output element
#[derive(Debug)]
pub enum SoundGen {
    /// emits a stub sound when it's time to start playing, doesn't accept any input
    Input{base: Element},
    /// generates a wave combined from a variable number of primitive waves of customizable form
    Wave{base: Element, waves: Vec<RawWaveDef>, n_waves: usize},
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

    /// the returned optional integer marks the interval (in microseconds) after which
    /// this function should be called again with a stub sound as input
    pub fn transform(&mut self, player: &mut SoundPlayer, sound: Sound) -> JsResult<(Sound, Option<u32>)> {
        match self {
            Self::Wave{waves, ..} =>
                sound.wave(player.audio_ctx(), waves.iter().map(|&x| x.into()))
                    .map(|x| (x, None)).add_loc(loc!()),
            &mut Self::Envelope{attack, decay, sustain, release, ..} =>
                sound.envelope(player.audio_ctx(), attack, decay, sustain, release)
                    .map(|x| (x, None)).add_loc(loc!()),
            Self::Output{..} =>
                player.play_sound(sound).map(|_| (Sound::End, None)).add_loc(loc!()),
            Self::Input{..} => Ok((sound, None))
        }
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
            Self::Wave{waves, n_waves, ..} => html!{<div id="inputs" style="grid-template-columns:repeat(3,1fr)">
                {for waves.iter().enumerate().map(|(i, wave)| html!{<>
                    <div id="wave-options">
                        <Button
                        key={wave.id * 5 + 2}
                        id={ParamId::RemoveWave(self.id, i as u32)}
                        desc={"Remove wave element"}>
                            <div>{"Remove"}</div>
                        </Button>
                        <Button
                        key={wave.id * 5 + 3}
                        id={ParamId::ToggleWaveToneType(self.id, i as u32)}
                        desc={"Toggle tone input mode"}>
                            <div>{if let Tone::Freq(_) = wave.tone {"Frequency"} else {"Pitch"}}</div>
                        </Button>
                    </div>
                    if let Tone::Note(note) = wave.tone {
                        <Switch
                        key={wave.id * 5}
                        id={ParamId::WaveTone(self.id, i as u32)}
                        options={Note::NAMES.to_vec()}
                        name={"Note"}
                        initial={note.to_index()}/>
                    } else if let Tone::Freq(freq) = wave.tone {
                        <Slider
                        key={wave.id * 5}
                        id={ParamId::WaveTone(self.id, i as u32)}
                        coef={Sound::MAX_WAVE_FREQ} precision={0}
                        postfix={"Hz"}
                        name={"Frequency"}
                        initial={freq as f64}/>
                    }
                    <Switch
                    key={wave.id * 5 + 1}
                    id={ParamId::WaveType(self.id, i as u32)}
                    options={Sound::WAVE_TYPE_NAMES.to_vec()}
                    name={"Wave type"}
                    initial={Sound::WAVE_TYPES.iter().position(|&x| x == wave.wave_type).unwrap_or(0)}/>
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
        Ok(match self {
            Self::Wave{waves, n_waves, ..} => match id {
                ParamId::ToggleWaveToneType(_, id) => value.is_sign_negative().then_try(|| {
                    waves.get_mut_or_js_error(id as usize, "wave element #", " not found").add_loc(loc!())
                        .map(|wave| wave.tone = wave.tone.toggled())
                })?.is_some(),

                ParamId::WaveTone(_, id) => {
                    if !value.is_finite() || value < 0.0 {
                        js_error(format!("{:?} is an invalid wave tone", value), loc!())?;
                    }
                    match waves.get_mut_or_js_error(id as usize, "wave element #", " not found").add_loc(loc!())?.tone {
                        Tone::Note(ref mut note) => *note = Note::index(value as usize)
                            .to_js_result_with(|| format!("{:?} is an invalid note index", value)).add_loc(loc!())?,
                        Tone::Freq(ref mut freq) => *freq = value as f32
                    }
                    false}

                ParamId::WaveType(_, id) => {
                    waves.get_mut_or_js_error(id as usize, "wave element #", " not found").add_loc(loc!())?
                        .wave_type = *Sound::WAVE_TYPES
                            .get_or_js_error(value as usize, "wave type #", " not found").add_loc(loc!())?;
                    false}

                ParamId::RemoveWave(_, id) => value.is_sign_negative().then(||
                    waves.remove(id as usize)).is_some(),

                ParamId::AddWave(_) => value.is_sign_negative().then(|| {
                    waves.push(RawWaveDef::new(*n_waves));
                    *n_waves += 1;
                }).is_some(),

                id => js_error(format!("`{}` sound element has no parameter `{:?}`", self.name(), id), loc!())?
            }

            Self::Envelope{attack, decay, sustain, release, ..} => match id {
                ParamId::EnvelopeAttack(_) =>  {*attack  = value as u32; false}
                ParamId::EnvelopeDecay(_)  =>  {*decay   = value as u32; false}
                ParamId::EnvelopSustain(_) =>  {*sustain = value as f32; false}
                ParamId::EnvelopeRelease(_) => {*release = value as u32; false}
                id => js_error(format!("`{}` sound element has no parameter `{:?}`", self.name(), id), loc!())?
            }

            x => js_error(format!("cannot set a parameter on a `{}` sound element", x.name()), loc!())?
        })
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
    audio_ctx: AudioContext
}

impl SoundPlayer {
    pub fn new() -> JsResult<Self> {
        let audio_ctx = AudioContext::new().add_loc(loc!())?;
        let output = AnalyserNode::new(&audio_ctx).add_loc(loc!())?;
        output.connect_with_audio_node(&audio_ctx.destination()).add_loc(loc!())?;
        Ok(Self{sounds: vec![], ending_all: false, output, audio_ctx})
    }

    pub fn play_sound(&mut self, sound: Sound) -> JsResult<()> {
        Ok(if matches!(sound, Sound::End) {
            self.ending_all = true;
        } else {
            self.sounds.push((sound.prepare(&self.output).add_loc(loc!())?, SoundState::Active(0.0)));
        })
    }

    #[inline] pub fn audio_ctx(&self) -> &AudioContext {&self.audio_ctx}

    #[inline] pub fn output(&self) -> &AnalyserNode {&self.output}

    pub fn poll(&mut self, time: f64) -> JsResult<()> {
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
        err
    }
}
