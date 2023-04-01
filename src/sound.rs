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
        JsResultUtils, BoolExt, SliceExt, VecExt, js_error},
    input::{Switch, Slider, Button, ParamId},
    MainCmd,
    loc, SoundEvent};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Note(u8);

impl Note {
    pub const C2:  Note = Note(0);
    pub const CS2: Note = Note(1);
    pub const D2:  Note = Note(2);
    pub const DS2: Note = Note(3);
    pub const E2:  Note = Note(4);
    pub const F2:  Note = Note(5);
    pub const FS2: Note = Note(6);
    pub const G2:  Note = Note(7);
    pub const GS2: Note = Note(8);
    pub const A2:  Note = Note(9);
    pub const AS2: Note = Note(10);
    pub const B2:  Note = Note(11);
    pub const C3:  Note = Note(12);
    pub const CS3: Note = Note(13);
    pub const D3:  Note = Note(14);
    pub const DS3: Note = Note(15);
    pub const E3:  Note = Note(16);
    pub const F3:  Note = Note(17);
    pub const FS3: Note = Note(18);
    pub const G3:  Note = Note(19);
    pub const GS3: Note = Note(20);
    pub const A3:  Note = Note(21);
    pub const AS3: Note = Note(22);
    pub const B3:  Note = Note(23);
    pub const C4:  Note = Note(24);
    pub const CS4: Note = Note(25);
    pub const D4:  Note = Note(26);
    pub const DS4: Note = Note(27);
    pub const E4:  Note = Note(28);
    pub const F4:  Note = Note(29);
    pub const FS4: Note = Note(30);
    pub const G4:  Note = Note(31);
    pub const GS4: Note = Note(32);
    pub const A4:  Note = Note(33);
    pub const AS4: Note = Note(34);
    pub const B4:  Note = Note(35);

    pub const MAX: Note = Note::B4;
    pub const N_OCTAVES: usize = 3;

    pub const ALL: [Note; 36] = {
        let mut res = [Note(0); 36];
        let mut iter = 0;
        while iter < res.len() {
            res[iter].0 = iter as u8;
            iter += 1;
        }
        res
    };

    pub const FREQS: [f32; 36] = [
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
        196.00/*G3*/, 207.65/*G#3*/,
        220.0 /*A3*/, 233.08/*A#3*/,
        246.94/*B3*/,
        261.63/*C4*/, 277.18/*C#4*/,
        293.66/*D4*/, 311.13/*D#4*/,
        329.63/*E4*/,
        349.23/*F4*/, 369.99/*F#4*/,
        392.00/*G4*/, 415.30/*G#4*/,
        440.0 /*A4*/, 466.16/*A#4*/,
        493.88/*B4*/
    ];

    pub const NAMES: [&'static str; 36] = [
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
        "G3", "G#3",
        "A3", "A#3",
        "B3",
        "C4", "C#4",
        "D4", "D#4",
        "E4",
        "F4", "F#4",
        "G4", "G#4",
        "A4", "A#4",
        "B4"];

    #[inline] pub const fn from_index(value: usize) -> Option<Self> {
        if value >= Self::FREQS.len() {return None}
        Some(Self(value as u8))
    }

    /// SAFETY: `value` must be finite
    pub unsafe fn from_freq_unchecked(value: f32) -> Self {
        const MAX: usize = Note::FREQS.len() - 1;
        Self(Self::FREQS.iter()
            .position(|&freq| value <= freq)
            .unwrap_or(MAX) as u8)
    }

    #[inline] pub const fn name(&self) -> &'static str {
        unsafe{Self::NAMES.get_unchecked(self.0 as usize)}
    }

    /*pub fn freq(value: f32) -> Option<Self> {
        if !value.is_finite() {return None}
        Some(unsafe{Self::freq_unchecked(value)})
    }*/

    #[inline] pub const fn index(&self) -> usize {
        self.0 as usize
    }

    pub fn diatonic_index(&self) -> usize {
        let octave = self.0 as usize / 12;
        let pitch = [0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6].get_wrapping(self.0 as usize);
        octave * 7 + *pitch
    }

    #[inline] pub const fn freq(&self) -> f32 {
        unsafe{*Self::FREQS.get_unchecked(self.0 as usize)}
    }

    #[inline] pub const fn is_sharp(&self) -> bool {
        self.0 % 2 == (self.0 % 12 < 5) as u8
    }
}

#[derive(Debug, Clone, Copy)]
enum Pitch {
    Freq(f32),
    Note(i8)
}

impl Pitch {
    fn toggle(&mut self) {
        *self = match *self {
            Self::Freq(_) => Self::Note(0),
            Self::Note(_) => Self::Freq(0.0)}
    }

    fn apply(&self, note: Note) -> f32 {
        match self {
            Self::Freq(freq) => note.freq() + freq,
            Self::Note(off)  =>
                Note(note.0.saturating_add_signed(*off).min(Note::MAX.0)).freq()
        }
    }
}

#[derive(Clone, Debug)]
pub enum Sound {
    InputNote(Note),
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

    fn wave<'a>(self, ctx: &AudioContext, waves: impl Iterator<Item = WaveDef>) -> JsResult<Self> {
        Ok(match self {
            Self::InputNote(note) => {
                let ctrl = GainNode::new_with_options(ctx, GainOptions::new().gain(f32::MIN_POSITIVE)).add_loc(loc!())?;
                for wave in waves {
                    let wave = OscillatorNode::new_with_options(ctx, OscillatorOptions::new()
                        .frequency(wave.pitch.apply(note))
                        .type_(wave.wave_type)).add_loc(loc!())?;
                    wave.start().add_loc(loc!())?;
                    wave.connect_with_audio_node(&ctrl).add_loc(loc!())?;
                }
                Self::Wave(ctrl)}
            x @Self::End => x,
            x => js_error(format!("`{}` sound cannot be turned into a `Complex Wave`", x.name()), loc!())?
        })
    }

    /// `attack`, `decay` and `release` are in milliseconds, `sustain` must be in [0; 1]
    fn envelope(self, ctx: &AudioContext, attack: u32, decay: u32, sustain: f32, release: u32) -> JsResult<Self> {
        let ctrl = match self {
            Self::InputNote(note) => {
                let ctrl = GainNode::new_with_options(ctx, GainOptions::new().gain(f32::MIN_POSITIVE)).add_loc(loc!())?;
                let gen = OscillatorNode::new_with_options(ctx,
                    OscillatorOptions::new().frequency(note.freq())).add_loc(loc!())?;
                gen.connect_with_audio_node(&ctrl).add_loc(loc!())?;
                gen.start().add_loc(loc!())?;
                ctrl}
            Self::Wave(x) => x,
            Self::Envelope{ctrl, ..} => ctrl,
            x @Self::End => return Ok(x)};
        Ok(Self::Envelope{attack: Some(attack), decay, sustain, release, ctrl})
    }

    #[inline] fn name(&self) -> &'static str {
        match self {
            Self::InputNote(_) => "Input Note",
            Self::Wave(_) => "Complex Wave",
            Self::Envelope{..} => "Envelope",
            Self::End => "Ending Signal"}
    }

    fn prepare(self, dest: &AudioNode) -> JsResult<Self> {
        Ok(match self {
            Self::InputNote(note) => {
                let ctx = dest.context();
                let ctrl = GainNode::new_with_options(&ctx, GainOptions::new()
                    .gain(f32::MIN_POSITIVE)).add_loc(loc!())?;
                OscillatorNode::new_with_options(&ctx, OscillatorOptions::new()
                    .frequency(note.freq())).add_loc(loc!())?
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

    fn progress(&mut self) -> JsResult<Option<u32>> {
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

    /// returns the time interval in milliseconds after which the object can be discarded
    fn end(&mut self) -> JsResult<u32> {
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

    fn disconnect(self) -> JsResult<()> {
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
pub struct WaveDef {
    id: usize,
    pitch: Pitch,
    wave_type: OscillatorType
}

impl WaveDef {
    fn new(id: usize) -> Self {
        Self{id, pitch: Pitch::Freq(0.0), wave_type: OscillatorType::Sine}
    }
}

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

    /// the returned optional integer marks the interval (in microseconds) after which
    /// this function should be called again with a stub sound as input
    pub fn transform(&mut self, player: &mut SoundPlayer, sound: Sound, _time: f64) -> JsResult<(Sound, Option<SoundEvent>)> {
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
                        id={ParamId::ToggleWavePitchType(self.id, i as u32)}
                        desc={"Toggle pitch input mode"}>
                            <div>{if let Pitch::Freq(_) = wave.pitch {"Frequency"} else {"Note"}}</div>
                        </Button>
                    </div>
                    if let Pitch::Note(off) = wave.pitch {
                        <Slider
                        key={wave.id * 5}
                        id={ParamId::WavePitch(self.id, i as u32)}
                        max={Note::MAX.0 as f64} precision={0}
                        signed={true}
                        name={"Note"}
                        initial={off as f64}/>
                    } else if let Pitch::Freq(off) = wave.pitch {
                        <Slider
                        key={wave.id * 5 + 4}
                        id={ParamId::WavePitch(self.id, i as u32)}
                        max={Sound::MAX_WAVE_FREQ} precision={0}
                        signed={true}
                        postfix={"Hz"}
                        name={"Frequency"}
                        initial={off as f64}/>
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
                desc="Add new wave element"
                class="add-wave-button">
                    <svg viewBox="0 0 100 100" style="height:100%">
                        <polygon points="45,25 55,25 55,45 75,45 75,55 55,55 55,75 45,75 45,55 25,55 25,45 45,45"/>
                    </svg>
                </Button>
            </div>},

            Self::Envelope{attack, decay, sustain, release, ..} => html! {<div id="inputs">
                <Slider name={"Attack time"}
                    id={ParamId::EnvelopeAttack(self.id)}
                    max={Sound::MAX_INTERVAL}
                    postfix={"ms"} precision={0}
                    initial={*attack as f64}/>
                <Slider name={"Decay time"}
                    id={ParamId::EnvelopeDecay(self.id)}
                    max={Sound::MAX_INTERVAL}
                    postfix={"ms"} precision={0}
                    initial={*decay as f64}/>
                <Slider name={"Sustain level"}
                    id={ParamId::EnvelopSustain(self.id)}
                    max={1.0}
                    postfix={""} precision={2}
                    initial={*sustain as f64}/>
                <Slider name={"Release time"}
                    id={ParamId::EnvelopeRelease(self.id)}
                    max={Sound::MAX_INTERVAL}
                    postfix={"ms"} precision={0}
                    initial={*release as f64}/>
            </div>},
            _ => Default::default()}
    }

    /// the returned boolean marks whether the sound element's editor window should be rerendered
    pub fn set_param(&mut self, id: ParamId, value: f64) -> JsResult<bool> {
        Ok(match self {
            Self::Wave{waves, n_waves, ..} => match id {
                ParamId::ToggleWavePitchType(_, id) => value.is_sign_negative().then_try(|| {
                    waves.get_mut_or_js_error(id as usize, "wave element #", " not found").add_loc(loc!())
                        .map(|wave| wave.pitch.toggle())
                })?.is_some(),

                ParamId::WavePitch(_, id) => {
                    match waves.get_mut_or_js_error(id as usize, "wave element #", " not found").add_loc(loc!())?.pitch {
                        Pitch::Note(ref mut off) => *off = value as i8,
                        Pitch::Freq(ref mut off) => *off = value as f32}
                    false}

                ParamId::WaveType(_, id) => {
                    waves.get_mut_or_js_error(id as usize, "wave element #", " not found").add_loc(loc!())?
                        .wave_type = *Sound::WAVE_TYPES
                            .get_or_js_error(value as usize, "wave type #", " not found").add_loc(loc!())?;
                    false}

                ParamId::RemoveWave(_, id) => value.is_sign_negative().then(||
                    waves.remove(id as usize)).is_some(),

                ParamId::AddWave(_) => value.is_sign_negative().then(|| {
                    waves.push(WaveDef::new(*n_waves));
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
