use std::{
    ops::{Not, Deref, DerefMut, Range, Add, Sub, Neg},
    fmt::{self, Display, Formatter},
    cmp::{Ordering, Reverse}, iter::successors};
use web_sys::{
    AudioNode, AnalyserNode,
    AudioContext,
    GainNode, GainOptions,
    OscillatorNode, OscillatorOptions, OscillatorType,
    CanvasRenderingContext2d, DynamicsCompressorNode, DynamicsCompressorOptions};
use yew::{html, Html};
use crate::{
    utils::{
        JsResult,
        Point,
        HitZone,
        HorizontalArrow,
        Rhombus,
        JsResultUtils,
        BoolExt,
        SliceExt,
        VecExt,
        js_error,
        Pipe,
        OptionExt,
        ResultToJsResult,
        R64, R32,
        SaturatingInto,
        RangeExt, Check},
    input::{Switch, Slider, Button, ParamId},
    MainCmd,
    loc,
    SoundEvent, r32, r64};

type MSecs = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, PartialOrd, Ord)]
pub struct Note(u8);

impl Display for Note {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", unsafe{Self::NAMES.get_unchecked(self.0 as usize)})
    }
}

impl Add<isize> for Note {
    type Output = Note;
    #[inline] fn add(self, rhs: isize) -> Self::Output {
        Self(self.0.saturating_add_signed(rhs.saturating_into()))
    }
}

impl Sub<isize> for Note {
    type Output = Note;
    #[inline] fn sub(self, rhs: isize) -> Self::Output {
        Self(self.0.saturating_add_signed(rhs.neg().saturating_into()))
    }
}

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

    pub const FREQS: [R32; 36] = [
        r32![65.410] /*C2*/, r32![69.300] /*C#2*/,
        r32![73.420] /*D2*/, r32![77.780] /*D#2*/,
        r32![82.410] /*E2*/,
        r32![87.310] /*F2*/, r32![92.500] /*F#2*/,
        r32![98.000] /*G2*/, r32![103.83] /*G#2*/,
        r32![110.00] /*A2*/, r32![116.54] /*A#2*/,
        r32![123.47] /*B2*/,
        r32![130.81] /*C3*/, r32![138.59] /*C#3*/,
        r32![146.83] /*D3*/, r32![155.56] /*D#3*/,
        r32![164.81] /*E3*/,
        r32![174.61] /*F3*/, r32![185.00] /*F#3*/,
        r32![196.00] /*G3*/, r32![207.65] /*G#3*/,
        r32![220.00] /*A3*/, r32![233.08] /*A#3*/,
        r32![246.94] /*B3*/,
        r32![261.63] /*C4*/, r32![277.18] /*C#4*/,
        r32![293.66] /*D4*/, r32![311.13] /*D#4*/,
        r32![329.63] /*E4*/,
        r32![349.23] /*F4*/, r32![369.99] /*F#4*/,
        r32![392.00] /*G4*/, r32![415.30] /*G#4*/,
        r32![440.00] /*A4*/, r32![466.16] /*A#4*/,
        r32![493.88] /*B4*/
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

    #[inline] pub const fn from_index(value: usize) -> Self {
        if value >= Self::FREQS.len() {Self::MAX}
        else {Self(value as u8)}
    }

    /// SAFETY: `value` must be finite
    pub unsafe fn from_freq_unchecked(value: R32) -> Self {
        const MAX: usize = Note::FREQS.len() - 1;
        Self(Self::FREQS.iter()
            .position(|&freq| value <= freq)
            .unwrap_or(MAX) as u8)
    }

    #[inline] pub const fn name(&self) -> &'static str {
        unsafe{Self::NAMES.get_unchecked(self.0 as usize)}
    }

    /*pub fn from_freq(value: R32) -> Option<Self> {
        if !value.is_finite() {return None}
        Some(unsafe{Self::from_freq_unchecked(value)})
    }*/

    #[inline] pub const fn index(&self) -> usize {
        self.0 as usize
    }

    pub fn diatonic_index(&self) -> usize {
        let octave = self.0 as usize / 12;
        let pitch = [0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6].get_wrapping(self.0 as usize);
        octave * 7 + *pitch
    }

    #[inline] pub const fn freq(&self) -> R32 {
        unsafe{*Self::FREQS.get_unchecked(self.0 as usize)}
    }

    #[inline] pub const fn is_sharp(&self) -> bool {
        self.0 % 2 == (self.0 % 12 < 5) as u8
    }
}

#[derive(Debug, Clone, Copy)]
enum Pitch {
    Freq(R32),
    Note(i8)
}

impl Pitch {
    fn toggle(&mut self) {
        *self = match *self {
            Self::Freq(_) => Self::Note(0),
            Self::Note(_) => Self::Freq(R32::ZERO)}
    }

    fn apply(&self, note: Note) -> R32 {
        match self {
            Self::Freq(freq) => note.freq() + *freq,
            Self::Note(off)  =>
                Note(note.0.saturating_add_signed(*off).min(Note::MAX.0)).freq()
        }
    }
}

#[derive(Clone, Debug)]
pub enum Sound {
    InputNote(Note, Option<MSecs>),
    Wave(GainNode, Option<MSecs>),
    Envelope{attack: MSecs, decay: MSecs, sustain_level: R32, sustain: Option<MSecs>, release: MSecs, ctrl: GainNode},
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
    const MAX_WAVE_FREQ: R64 = r64![5000.0];
    const MAX_INTERVAL: R64 = r64![2000.0];

    fn wave<'a>(self, ctx: &AudioContext, waves: &[WaveDef]) -> JsResult<Self> {
        Ok(match self {
            Self::InputNote(note, duration) => {
                let ctrl = GainNode::new_with_options(ctx, GainOptions::new().gain(f32::MIN_POSITIVE)).add_loc(loc!())?;
                for wave in waves {
                    let wave = OscillatorNode::new_with_options(ctx, OscillatorOptions::new()
                        .frequency(*wave.pitch.apply(note))
                        .type_(wave.wave_type)).add_loc(loc!())?;
                    wave.start().add_loc(loc!())?;
                    wave.connect_with_audio_node(&ctrl).add_loc(loc!())?;
                }
                Self::Wave(ctrl, duration)}
            x @Self::End => x,
            x => js_error(format!("`{}` sound cannot be turned into a `Complex Wave`", x.name()), loc!())?
        })
    }

    /// `sustain_level` must be in [0; 1]
    fn envelope(self, ctx: &AudioContext, mut attack: MSecs, mut decay: MSecs, sustain_level: R32, release: MSecs) -> JsResult<Self> {
        let (ctrl, duration) = match self {
            Self::InputNote(note, duration) => {
                let ctrl = GainNode::new_with_options(ctx, GainOptions::new().gain(f32::MIN_POSITIVE)).add_loc(loc!())?;
                let gen = OscillatorNode::new_with_options(ctx,
                    OscillatorOptions::new().frequency(*note.freq())).add_loc(loc!())?;
                gen.connect_with_audio_node(&ctrl).add_loc(loc!())?;
                gen.start().add_loc(loc!())?;
                (ctrl, duration)}
            Self::Wave(x, duration) => (x, duration),
            Self::Envelope{ctrl, attack, decay, sustain, ..} => (ctrl, sustain.map(|x| x + attack + decay)),
            x @Self::End => return Ok(x)};
        let mut sustain = None;
        if let Some(duration) = duration {
            sustain = duration.saturating_sub(attack).saturating_sub(decay).into();
            decay = duration.min(attack + decay).saturating_sub(attack);
            attack = duration.min(attack);
        }
        // TODO: add volume cut-off to the attack phase if it's longer than the whole sound's
        // duration
        Ok(Self::Envelope{attack, decay, sustain_level, sustain, release, ctrl})
    }

    #[inline] fn name(&self) -> &'static str {
        match self {
            Self::InputNote(..) => "Input Note",
            Self::Wave(..) => "Complex Wave",
            Self::Envelope{..} => "Envelope",
            Self::End => "Ending Signal"}
    }

    fn prepare(self, dest: &AudioNode) -> JsResult<Self> {
        Ok(match self {
            Self::InputNote(note, duration) => {
                let ctx = dest.context();
                let ctrl = GainNode::new_with_options(&ctx, GainOptions::new()
                    .gain(f32::MIN_POSITIVE)).add_loc(loc!())?;
                OscillatorNode::new_with_options(&ctx, OscillatorOptions::new()
                    .frequency(*note.freq())).add_loc(loc!())?
                    .connect_with_audio_node(&ctrl).add_loc(loc!())?
                    .connect_with_audio_node(dest).add_loc(loc!())?;
                Self::Wave(ctrl, duration)}

            Self::Wave(ctrl, duration) => {
                ctrl.connect_with_audio_node(dest).add_loc(loc!())?;
                Self::Wave(ctrl, duration)}

            Self::Envelope{attack, decay, sustain_level, sustain, release, ctrl} => {
                ctrl.connect_with_audio_node(dest).add_loc(loc!())?;
                Self::Envelope{attack, decay, sustain_level, sustain, release, ctrl}}

            x => js_error(format!("`{}` sound cannot be played", x.name()), loc!())?
        })
    }

    /// `state` is an integer that had been passed to the `SoundState` that prompted the call
    /// of this method, on the first call it's set to 0
    fn progress(&self, time: R64, state: u32) -> JsResult<SoundState> {
        Ok(match self {
            &Self::Wave(ref ctrl, duration) => if state == 0 {
                let gain = ctrl.gain();
                gain.set_value(1.0);
                let at = if let Some(duration) = duration {
                    let at = duration as f64 / 1000.0 + *time;
                    gain.set_value_at_time(f32::MIN_POSITIVE, at).add_loc(loc!())?;
                    unsafe{R64::new_unchecked(at)}
                } else {R64::INFINITY};
                SoundState::Active(at, 1)
            } else {SoundState::Ending(time)}

            &Self::Envelope{attack, decay, sustain, sustain_level, release, ref ctrl} => if state == 0 {
                let gain = ctrl.gain();
                let mut at = match (attack, decay) {
                    (0, 0) => {
                        gain.set_value(*sustain_level);
                        *time}

                    (0, decay) => {
                        gain.set_value(1.0);
                        let at = decay as f64 / 1000.0 + *time;
                        gain.linear_ramp_to_value_at_time(*sustain_level, at).add_loc(loc!())?;
                        at}

                    (attack, 0) => {
                        let at = attack as f64 / 1000.0 + *time;
                        gain.linear_ramp_to_value_at_time(1.0, at).add_loc(loc!())?;
                        gain.set_value_at_time(*sustain_level, at).add_loc(loc!())?;
                        at}

                    (attack, decay) => {
                        let mut at = attack as f64 / 1000.0 + *time;
                        gain.linear_ramp_to_value_at_time(1.0, at).add_loc(loc!())?;
                        at += decay as f64 / 1000.0;
                        gain.linear_ramp_to_value_at_time(*sustain_level, at).add_loc(loc!())?;
                        at}
                };
                if let Some(sustain) = sustain {
                    at += sustain as f64 / 1000.0;
                    gain.set_value_at_time(*sustain_level, at).add_loc(loc!())?;
                    gain.linear_ramp_to_value_at_time(f32::MIN_POSITIVE, release as f64 / 1000.0 + at).add_loc(loc!())?;
                    SoundState::Active(at.try_into().to_js_result(loc!())?, 1)
                } else {SoundState::Active(R64::INFINITY, 1)}
            } else {SoundState::Ending(time + R64::from(release) / 1000)}

            x => js_error(format!("`{}` sound cannot be played", x.name()), loc!())?
        })
    }

    /// returns the time interval after which the object can be discarded
    fn end(&self, time: R64) -> JsResult<MSecs> {
        Ok(match self {
            Self::Wave(ctrl, _) =>
                ctrl.gain().cancel_scheduled_values(0.0).add_loc(loc!())?
                    .set_value(f32::MIN_POSITIVE).pipe(|_| 0),

            &Self::Envelope{release, ref ctrl, ..} => {
                ctrl.gain().cancel_scheduled_values(0.0).add_loc(loc!())?
                    .linear_ramp_to_value_at_time(f32::MIN_POSITIVE, release as f64 / 1000.0 + *time).add_loc(loc!())?;
                release}

            x => js_error(format!("`{}` sound cannot be played", x.name()), loc!())?
        })
    }

    fn disconnect(self) -> JsResult<()> {
        match self {
            Self::Wave(ctrl, _) | Self::Envelope{ctrl, ..}
                => ctrl.disconnect().add_loc(loc!()),
            _ => Ok(())}
    }
}

#[derive(Debug, Default)]
enum Focus {
    #[default] None,
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
        Self{id, pitch: Pitch::Freq(r32![0.0]), wave_type: OscillatorType::Sine}
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct PatternBlock {
    note: Note,
    duration: MSecs,
    offset: MSecs
}

impl Ord for PatternBlock {
    fn cmp(&self, other: &Self) -> Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl PartialOrd for PatternBlock {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.offset.partial_cmp(&other.offset)
    }
}

impl PatternBlock {
    #[inline] fn span(&self) -> Range<MSecs> {
        self.offset .. self.offset + self.duration
    }
}

pub struct GraphEvent {
    pub shift: bool,
    pub alt: bool,
    pub point: Point
}

#[derive(Debug, Clone, Copy)]
pub enum PatternInputFocus {
    DragStart(usize, MSecs),
    DragEnd(usize, MSecs),
    Move(usize, MSecs),
    DragPlane(MSecs, i32),
    DragWrapPoint(MSecs)
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
    Envelope{base: Element, attack: u32, decay: u32, sustain_level: R32, release: u32},
    /// emits the input note in a pattern with given durations, intervals and pitches
    /// can only be connected to the input node (for now)
    Pattern{base: Element, pattern: Vec<PatternBlock>, cur_block_id: Option<usize>,
        input_focus: Option<PatternInputFocus>, last_duration: MSecs,
        pattern_offset: MSecs, displayed_interval: MSecs,
        wrap_point: MSecs},
    /// consumes the input sound, delegating it to the `SoundPlayer`
    Output{base: Element},
}

impl Deref for SoundGen {
    type Target = Element;
    fn deref(&self) -> &Self::Target {
        match self {
            Self::Input{base} => base,
            Self::Wave {base, ..} => base,
            Self::Envelope{base, ..} => base,
            Self::Pattern{base, ..} => base,
            Self::Output{base} => base}
    }
}

impl DerefMut for SoundGen {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Self::Input{base} => base,
            Self::Wave {base, ..} => base,
            Self::Envelope{base, ..} => base,
            Self::Pattern{base, ..} => base,
            Self::Output{base} => base}
    }
}

/// used to visualise certain sound elements while editing them
#[derive(Debug, Clone, Copy)]
pub struct GraphSpec {
    pub element_id: usize,
    pub ratio: R32,
    pub interactive: bool
}

impl SoundGen {
    const VISUAL_SIZE: i32 = 32;
    const PATTERN_DEFAULT_BLOCK_DURATION: MSecs = 50;
    #[inline] pub fn new_input(location: Point) -> Self {
        Self::Input{base: Element{location, focus: Focus::None, id: 0}}
    }

    #[inline] pub fn new_wave(location: Point) -> Self {
        Self::Wave{waves: vec![], n_waves: 0,
            base: Element{location, focus: Focus::None, id: 0}}
    }

    #[inline] pub fn new_envelope(location: Point) -> Self {
        Self::Envelope{attack: 0, decay: 0, sustain_level: r32![0.0], release: 0,
            base: Element{location, focus: Focus::None, id: 0}}
    }

    #[inline] pub fn new_pattern(location: Point) -> Self {
        Self::Pattern{pattern: vec![], cur_block_id: None,
            input_focus: None, last_duration: 50, wrap_point: 1000,
            pattern_offset: 0, displayed_interval: 1000,
            base: Element{location, focus: Focus::None, id: 0}}
    }

    #[inline] pub fn new_output(location: Point) -> Self {
        Self::Output{base: Element{location, focus: Focus::None, id: 0}}
    }

    #[inline] pub fn id(&self) -> usize {self.id}
    #[inline] pub fn set_id(&mut self, id: usize) {self.id = id}

    #[inline] pub fn name(&self) -> &'static str {
        match self {
            Self::Input{..} => "User Input",
            Self::Wave{..} => "Wave generator",
            Self::Envelope{..} => "Envelope",
            Self::Pattern{..} => "Pattern",
            Self::Output{..} => "Output"}
    }

    #[inline] pub fn graph_spec(&self) -> Option<GraphSpec> {
        match self {
            Self::Envelope{base, ..} =>
                Some(GraphSpec{element_id: base.id, ratio: r32![0.5], interactive: false}),
            Self::Pattern{base, ..} =>
                Some(GraphSpec{element_id: base.id, ratio: r32![1.5], interactive: true}),
            _ => None}
    }

    /// `self` is the source, `other` is the destination
    pub fn connectible(&self, other: &Self) -> bool {
        if self.id == other.id || matches!(self, SoundGen::Output{..}) || matches!(other, SoundGen::Input{..}) {
            return false
        }
        if matches!(other, SoundGen::Pattern{..}) && !matches!(self, SoundGen::Input{..}) {
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

    pub fn transform(&mut self, player: &mut SoundPlayer, sound: Sound, time: R64) -> JsResult<(Option<Sound>, Option<SoundEvent>)> {
        Ok(match self {
            Self::Wave{waves, ..} => (Some(sound.wave(player.audio_ctx(), waves).add_loc(loc!())?), None),

            &mut Self::Envelope{attack, decay, sustain_level, release, ..} =>
                (Some(sound.envelope(player.audio_ctx(), attack, decay, sustain_level, release).add_loc(loc!())?),
                    None),

            Self::Pattern{base, pattern, cur_block_id, wrap_point, ..} => if matches!(sound, Sound::End) {
                *cur_block_id = pattern.first().map_or(false, |x| x.offset == 0).then_some(0);
                (Some(Sound::End), None)
            } else {
                let Some(cur_block_id) = cur_block_id else {
                    return if let Some(when) = pattern.first().map(|x| time + x.offset) {
                        *cur_block_id = Some(0);
                        Ok((None, Some(SoundEvent{element_id: base.id, when})))
                    } else {Ok((None, None))}
                };
                let cur = unsafe{pattern.get_unchecked(*cur_block_id)};
                let (next_id, offset) = pattern.get(*cur_block_id + 1)
                    .map_or((0, unsafe{pattern.get_unchecked(0)}.offset + *wrap_point),
                        |x| (*cur_block_id + 1, x.offset));
                *cur_block_id = next_id;
                (Some(Sound::InputNote(cur.note, cur.duration.into())),
                    Some(SoundEvent{element_id: base.id, when: time + offset - cur.offset}))
            }

            Self::Output{..} => (player.play_sound(sound).map(|_| None).add_loc(loc!())?, None),

            Self::Input{..} => (Some(sound), None)})
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
                        desc="Remove wave element">
                            <div>{"Remove"}</div>
                        </Button>
                        <Button
                        key={wave.id * 5 + 3}
                        id={ParamId::ToggleWavePitchType(self.id, i as u32)}
                        desc="Toggle pitch input mode">
                            <div>{if let Pitch::Freq(_) = wave.pitch {"Frequency"} else {"Note"}}</div>
                        </Button>
                    </div>
                    if let Pitch::Note(off) = wave.pitch {
                        <Slider
                        key={wave.id * 5}
                        id={ParamId::WavePitch(self.id, i as u32)}
                        max={R64::from(Note::MAX.index())} precision={0}
                        signed={true}
                        name={"Note"}
                        initial={R64::from(off)}/>
                    } else if let Pitch::Freq(off) = wave.pitch {
                        <Slider
                        key={wave.id * 5 + 4}
                        id={ParamId::WavePitch(self.id, i as u32)}
                        max={Sound::MAX_WAVE_FREQ} precision={0}
                        signed={true}
                        postfix={"Hz"}
                        name={"Frequency"}
                        initial={R64::from(off)}/>
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

            Self::Envelope{attack, decay, sustain_level, release, ..} => html! {<div id="inputs">
                <Slider name={"Attack time"}
                    id={ParamId::EnvelopeAttack(self.id)}
                    max={Sound::MAX_INTERVAL}
                    postfix={"ms"} precision={0}
                    initial={R64::from(*attack)}/>
                <Slider name={"Decay time"}
                    id={ParamId::EnvelopeDecay(self.id)}
                    max={Sound::MAX_INTERVAL}
                    postfix={"ms"} precision={0}
                    initial={R64::from(*decay)}/>
                <Slider name={"Sustain level"}
                    id={ParamId::EnvelopSustain(self.id)}
                    max={r64![1.0]}
                    postfix={""} precision={2}
                    initial={R64::from(*sustain_level)}/>
                <Slider name={"Release time"}
                    id={ParamId::EnvelopeRelease(self.id)}
                    max={Sound::MAX_INTERVAL}
                    postfix={"ms"} precision={0}
                    initial={R64::from(*release)}/>
            </div>},

            _ => Default::default()}
    }

    /// the returned boolean marks whether the sound element's editor window should be rerendered
    pub fn set_param(&mut self, id: ParamId, value: R64) -> JsResult<bool> {
        Ok(match self {
            Self::Wave{waves, n_waves, ..} => match id {
                ParamId::ToggleWavePitchType(_, id) => value.is_sign_negative().then_try(|| {
                    waves.get_mut(id as usize).to_js_result(loc!())
                        .map(|wave| wave.pitch.toggle())
                })?.is_some(),

                ParamId::WavePitch(_, id) => {
                    match waves.get_mut(id as usize).to_js_result(loc!())?.pitch {
                        Pitch::Note(ref mut off) => *off = *value as i8,
                        Pitch::Freq(ref mut off) => *off = value.into()}
                    false}

                ParamId::WaveType(_, id) => {
                    waves.get_mut(id as usize).to_js_result(loc!())?
                        .wave_type = *Sound::WAVE_TYPES
                            .get(*value as usize).to_js_result(loc!())?;
                    false}

                ParamId::RemoveWave(_, id) => value.is_sign_negative().then(||
                    waves.remove(id as usize)).is_some(),

                ParamId::AddWave(_) => value.is_sign_negative().then(|| {
                    waves.push(WaveDef::new(*n_waves));
                    *n_waves += 1;
                }).is_some(),

                id => js_error(format!("`{}` sound element has no parameter `{:?}`", self.name(), id), loc!())?
            }

            Self::Envelope{attack, decay, sustain_level, release, ..} => match id {
                ParamId::EnvelopeAttack(_) =>  {*attack  = *value as u32; false}
                ParamId::EnvelopeDecay(_)  =>  {*decay   = *value as u32; false}
                ParamId::EnvelopSustain(_) =>  {*sustain_level = value.into(); false}
                ParamId::EnvelopeRelease(_) => {*release = *value as u32; false}
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

    pub fn graph(&mut self, width: u32, height: u32, ctx: &CanvasRenderingContext2d, event: Option<GraphEvent>) -> JsResult<()> {
        Ok(match self {
            &mut Self::Envelope {attack, decay, sustain_level, release, ..} => {
                let (width, height) = (width as f64, height as f64);
                let in_span = (attack + decay) as f64;
                let span = in_span + release as f64;
                ctx.move_to(0.0, height);
                ctx.line_to(attack as f64 / span * width, 0.0);
                ctx.line_to(in_span / span * width, (1.0 - *sustain_level) as f64 * height);
                ctx.line_to(width, height);
            }

            Self::Pattern{wrap_point, last_duration, input_focus, pattern, pattern_offset, displayed_interval, ..} => {
                if let Some(GraphEvent{shift, point, ..}) = event {
                    let offset = ((point.x as f32 / width as f32) * *displayed_interval as f32) as MSecs;
                    let note = Note::from_index(((1.0 - point.y as f32 / height as f32) * Note::ALL.len() as f32) as usize);
                    let some_input_focus = input_focus.get_or_insert_with(|| {
                        let span = (offset + *pattern_offset).pipe(|x| x - 5 .. x + 5);
                        let notes = note - 1 .. note + 1;
                        if let Some((index, &block)) = pattern.iter().enumerate().find(|(_, x)| notes.contains(&x.note) && x.span().overlap(&span)) {
                            /*
                            match offset f32 block.offset f32 - block.duration f32 / shift_pressed not f32 {
                                Infinite: index pattern remove |> offset DragPlane,
                                ..0.2: offset index DragStart,
                                0.8..: offset index DragEnd,
                                else: offset index Move
                            }
                            */
                            match ((offset + *pattern_offset) as f32 - block.offset as f32) / (block.duration * !shift as u32) as f32 {
                                x if x.is_infinite() => {pattern.remove(index); PatternInputFocus::DragPlane(offset, point.y)}
                                x if x < 0.2 => PatternInputFocus::DragStart(index, offset),
                                x if x > 0.8 => PatternInputFocus::DragEnd(index, offset),
                                _            => PatternInputFocus::Move(index, offset)
                            }
                        } else if (*wrap_point - 5 .. *wrap_point + 5).overlap(&span) {
                            PatternInputFocus::DragWrapPoint(offset)
                        } else if shift {
                            pattern.push_sorted(PatternBlock{note, duration: *last_duration, offset: offset + *pattern_offset})
                                .pipe(|x| PatternInputFocus::Move(x, offset))
                        } else {
                            PatternInputFocus::DragPlane(offset, point.y)
                        }
                    });
                    match some_input_focus {
                        PatternInputFocus::DragStart(index, last_offset) => {
                            let mut block = *unsafe{pattern.get_unchecked(*index)};
                            let delta = offset as i32 - *last_offset as i32;
                            block.offset = pattern.iter().take(*index).rev().find(|x| x.note == block.note)
                                .map_or(0, |x| (x.offset + x.duration) as i32)
                                .max(block.offset as i32 + delta) as u32;
                            block.duration = block.duration.saturating_add_signed(-delta);
                            *last_duration = block.duration;
                            *last_offset = offset;
                            if block.duration == 0 {
                                pattern.remove(*index);
                                *last_duration = Self::PATTERN_DEFAULT_BLOCK_DURATION;
                                *input_focus = None;
                            } else {
                                *index = pattern.set_sorted(*index, block).to_js_result(loc!())?;
                            }
                        }
                        PatternInputFocus::DragEnd(index, last_offset) => {
                            let mut block = *unsafe{pattern.get_unchecked(*index)};
                            let delta = offset as i32 - *last_offset as i32;
                            block.duration = pattern.iter().skip(*index + 1).find(|x| x.note == block.note)
                                .map_or(i32::MAX, |x| x.offset as i32 - block.offset as i32)
                                .min(block.duration as i32 + delta) as u32;
                            *last_duration = block.duration;
                            *last_offset = offset;
                            if block.duration == 0 {
                                pattern.remove(*index);
                                *last_duration = Self::PATTERN_DEFAULT_BLOCK_DURATION;
                                *input_focus = None;
                            } else {
                                *unsafe{pattern.get_unchecked_mut(*index)} = block;
                            }
                        }
                        PatternInputFocus::Move(index, last_offset) => {
                            let mut block = *unsafe{pattern.get_unchecked(*index)};
                            let delta = offset as i32 - *last_offset as i32;
                            block.note = note;
                            block.offset = i32::clamp(block.offset as i32 + delta,
                                pattern.iter().take(*index).rev().find(|x| x.note == block.note)
                                    .map_or(0, |x| (x.offset + x.duration) as i32),
                                pattern.iter().skip(*index + 1).find(|x| x.note == block.note)
                                    .map_or(*wrap_point as i32, |x| x.offset as i32) - block.duration as i32) as u32;
                            *index = pattern.set_sorted(*index, block).to_js_result(loc!())?;
                            *last_offset = offset;
                        }
                        PatternInputFocus::DragPlane(last_offset, last_y) => {
                            *pattern_offset = pattern_offset.saturating_add_signed(*last_offset as i32 - offset as i32);
                            *displayed_interval = displayed_interval.saturating_add_signed(*last_y - point.y as i32);
                            *last_offset = offset;
                            *last_y = point.y as i32;
                        }
                        PatternInputFocus::DragWrapPoint(last_offset) => {
                            *wrap_point = wrap_point.saturating_add_signed(offset as i32 - *last_offset as i32);
                            *last_offset = offset;
                        }
                    }
                } else {
                    *input_focus = None;
                }

                let (width, height) = (width as f64, height as f64);
                let note_height = -height / Note::ALL.len() as f64;
                ctx.rect((*wrap_point as f64 - *pattern_offset as f64) / *displayed_interval as f64 * width, 0.0,
                    2.0, height);
                let interval = 100.0 / *displayed_interval as f64 * width;
                let stroke_style = ctx.stroke_style();
                ctx.set_stroke_style(&"#232328".into());
                successors(Some(*pattern_offset as f64 / *displayed_interval as f64 * -width),
                    |x| (x + interval).check(|x| *x < width).ok())
                    .skip_while(|x| *x < 0.0)
                    .for_each(|i| ctx.stroke_rect(i, 0.0, 1.0, height));
                ctx.set_stroke_style(&stroke_style);
                pattern.iter()
                    .skip_while(|x| x.offset + x.duration < *pattern_offset)
                    .take_while(|x| x.offset < *pattern_offset + *displayed_interval)
                    .for_each(|block| ctx.rect(
                        (block.offset as f64 - *pattern_offset as f64) / *displayed_interval as f64 * width,
                        (1.0 - block.note.index() as f64 / Note::ALL.len() as f64) * height,
                        block.duration as f64 / *displayed_interval as f64 * width,
                        note_height))}

            _ => ()})
    }
}

#[derive(Clone, Copy, Debug,PartialEq, Eq, PartialOrd, Ord)]
enum SoundState {
    Active(R64, u32),
    Ending(R64),
}

impl Deref for SoundState {
    type Target = R64;
    fn deref(&self) -> &Self::Target {
        match self {
            Self::Active(x, _) => x,
            Self::Ending(x) => x}
    }
}

pub struct SoundPlayer {
    sounds: Vec<(Sound, SoundState)>,
    ending_all: bool,
    plug: DynamicsCompressorNode,
    visualiser: AnalyserNode,
    audio_ctx: AudioContext
}

impl SoundPlayer {
    const MASTER_GAIN: f32 = 0.2; // TODO: make customizable
    pub fn new() -> JsResult<Self> {
        let audio_ctx = AudioContext::new().add_loc(loc!())?;
        let plug = DynamicsCompressorNode::new_with_options(&audio_ctx,
            DynamicsCompressorOptions::new().ratio(20.0).release(1.0)).add_loc(loc!())?;
        let gain = GainNode::new_with_options(&audio_ctx,
            GainOptions::new().gain(Self::MASTER_GAIN)).add_loc(loc!())?;
        let visualiser = AnalyserNode::new(&audio_ctx).add_loc(loc!())?;

        plug.connect_with_audio_node(&visualiser).add_loc(loc!())?
            .connect_with_audio_node(&gain).add_loc(loc!())?
            .connect_with_audio_node(&audio_ctx.destination()).add_loc(loc!())?;
        Ok(Self{sounds: vec![], ending_all: false, plug, visualiser, audio_ctx})
    }

    pub fn play_sound(&mut self, sound: Sound) -> JsResult<()> {
        Ok(if matches!(sound, Sound::End) {
            self.ending_all = true;
        } else {
            self.sounds.push((sound.prepare(&self.plug).add_loc(loc!())?,
                SoundState::Active(r64![0.0], 0)));
        })
    }

    #[inline] pub fn audio_ctx(&self) -> &AudioContext {&self.audio_ctx}

    #[inline] pub fn visualiser(&self) -> &AnalyserNode {&self.visualiser}

    /// `time` is in seconds
    pub fn poll(&mut self, time: R64) -> JsResult<()> {
        if self.ending_all {
            self.ending_all = false;
            let mut err = Ok(()); // making sure to drop as many sounds as possible
            for (sound, state) in self.sounds.iter_mut() {
                if let SoundState::Ending(_) = state {continue}
                let after = match sound.end(time).add_loc(loc!()) {
                    Ok(after) => after,
                    Err(x) => {err = err.and(Err(x)); 0}};
                *state = SoundState::Ending(R64::from(after) / 1000 + time);
            }
            self.sounds.sort_unstable_by_key(|x| Reverse(x.1));
            err?;
        }

        let len = self.sounds.len();
        let start = match self.sounds.iter().rev().position(|&(_, state)| time < *state) {
            Some(x) => (x > 0).then(|| len - x),
            None => Some(0)};
        let mut err = Ok(());
        if let Some(start) = start {
            let pending: Vec<_> = self.sounds.drain(start..).collect();
            for (sound, mut state) in pending {
                match state {
                    SoundState::Active(_, state_id) => {
                        state = sound.progress(time, state_id).add_loc(loc!())?;
                        self.sounds.push_sorted_by_key((sound, state), |x| Reverse(x.1));
                    }
                    // not using the `?` operator to remove as much sounds as possible
                    SoundState::Ending(_) => err = err.and(sound.disconnect().add_loc(loc!()))
                }
            }
        }
        err
    }
}
