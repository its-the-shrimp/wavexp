use std::{
    ops::{Add, Sub, Neg},
    fmt::{self, Display, Formatter, Debug}};
use js_sys::Math::random;
use wasm_bindgen::{JsValue, JsCast};
use web_sys::{
    AudioNode,
    AudioContext,
    OscillatorNode, AudioBufferSourceNode, AudioBuffer, GainNode};
use yew::{html, Html};
use crate::{
    utils::{
        JsResult,
        JsResultUtils,
        R64, R32,
        SaturatingInto, Pipe},
    input::{Switch, ParamId, Slider},
    loc,
    r32, r64};

pub type MSecs = R64;
pub type Secs = R64;
pub type Beats = R64;

pub trait FromBeats {
    fn to_msecs(self, bps: Self) -> MSecs;
    fn to_secs(self, bps: Self) -> Secs;
    fn secs_to_beats(self, bps: Self) -> Beats;
}

impl FromBeats for Beats {
    #[inline]
    fn to_secs(self, bps: Self) -> Secs {self / bps}

    #[inline]
    fn to_msecs(self, bps: Self) -> MSecs {self / bps * r64![1000.0]}

    #[inline]
    fn secs_to_beats(self, bps: Self) -> Beats {self * bps}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, PartialOrd, Ord)]
pub struct Note(u8);

impl Display for Note {
    #[inline] fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    pub const MAX: Note = Note(35);

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

    #[inline] pub const fn index(&self) -> usize {
        self.0 as usize
    }

    #[inline] pub const fn freq(&self) -> R32 {
        unsafe{*Self::FREQS.get_unchecked(self.0 as usize)}
    }
}

pub struct TabInfo {
    pub name: &'static str
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SoundType {
    Note,
    Noise
}

impl SoundType {
    #[inline] pub fn name(&self) -> &'static str {
        match self {
            SoundType::Note => "Note",
            SoundType::Noise => "White Noise"
        }
    }

    pub fn desc(&self, offset: Beats, layer: i32) -> String {
        format!("{} @{:.3}, layer {}", self.name(), *offset, layer)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Sound {
    Note{gen: OscillatorNode, note: Note, len: Beats, started: bool,
        attack: Beats, release: Beats},
    Noise{gen: AudioBufferSourceNode, src: AudioBuffer,
        gain: GainNode, len: Beats, started: bool}
}

impl Sound {
    pub const TYPES: [SoundType; 2] = [
        SoundType::Note,
        SoundType::Noise
    ];

    #[inline] pub fn new(sound_type: SoundType, ctx: &AudioContext) -> JsResult<Self> {
        Ok(match sound_type {
            SoundType::Note => {
                let gen = ctx.create_oscillator().add_loc(loc!())?;
                gen.frequency().set_value(0.0);
                gen.start().add_loc(loc!())?;
                Self::Note{gen,
                    note: Note::MAX, len: r64![1.0], started: false,
                    attack: r64![0.4], release: r64![0.4]}
            }

            SoundType::Noise => {
                let len = ctx.sample_rate();
                let mut src_buf = vec![0.0f32; len as usize];
                src_buf.fill_with(|| random() as f32 * 2.0 - 1.0);
                let src = ctx.create_buffer(2, len as u32, len).add_loc(loc!())?;
                src.copy_to_channel(&src_buf, 0).add_loc(loc!())?;
                src.copy_to_channel(&src_buf, 1).add_loc(loc!())?;
                let gain = ctx.create_gain().add_loc(loc!())?;
                gain.gain().set_value(0.2);
                Self::Noise{gen: JsValue::NULL.unchecked_into(), src, gain,
                    len: r64![1.0], started: false}
            }
        })
    }

    #[inline] pub fn name(&self) -> &'static str {
        match self {
            Sound::Note{..} => "Note",
            Sound::Noise{..} => "Noise"
        }
    }

    #[inline] pub fn reset(&mut self, ctx: &AudioContext) -> JsResult<()> {
        Ok(match self {
            Sound::Note{started, gen, ..} => {
                *started = false;
                gen.frequency().cancel_scheduled_values(0.0).add_loc(loc!())?
                    .set_value(0.0);
            }

            Sound::Noise{gen, src, started, gain, ..} => {
                *started = false;
                *gen = ctx.create_buffer_source().add_loc(loc!())?;
                gen.set_loop(true);
                gen.set_buffer(Some(src));
                gen.start().add_loc(loc!())?;
                gen.connect_with_audio_node(gain).add_loc(loc!())?;
            }
        })
    }

    #[inline] pub fn poll(&mut self, time: Secs, plug: &AudioNode, bps: Beats) -> JsResult<Secs> {
        Ok(match self {
            Sound::Note{len, gen, started, note, attack, ..} => if *started {
                self.stop(time, bps).add_loc(loc!())?;
                Secs::INFINITY
            } else {
                *started = true;
                gen.connect_with_audio_node(plug).add_loc(loc!())?;
                gen.frequency()
                    .linear_ramp_to_value_at_time(*note.freq(), *time + *attack.to_secs(bps))
                    .add_loc(loc!())?;
                len.to_secs(bps) + time
            }

            Sound::Noise{gain, len, started, ..} => if *started {
                self.stop(time, bps).add_loc(loc!())?;
                Secs::INFINITY
            } else {
                *started = true;
                gain.connect_with_audio_node(plug).add_loc(loc!())?;
                len.to_secs(bps) + time
            }
        })
    }

    /// the method always properly stops the sound no matter if an error is returned or not
    #[inline] pub fn stop(&mut self, time: Secs, bps: Beats) -> JsResult<()> {
        match self {
            Sound::Note{gen, release, ..} =>
                gen.frequency()
                    .cancel_scheduled_values(0.0).add_loc(loc!())?
                    .linear_ramp_to_value_at_time(f32::MIN_POSITIVE, *time + *release.to_secs(bps))
                    .add_loc(loc!()).map(|_| ()),
            Sound::Noise{gen, ..} =>
                gen.stop().add_loc(loc!())
        }
    }

    #[inline] pub fn len(&self) -> Beats {
        match self {
            Sound::Note{len, ..}
            | Sound::Noise{len, ..} => *len
        }
    }

    #[inline] pub fn tabs(&self) -> &'static [TabInfo] {
        match self {
            Sound::Note{..} =>
                &[TabInfo{name: "General"}, TabInfo{name: "Pitch"}],
            Sound::Noise{..} =>
                &[TabInfo{name: "General"}, TabInfo{name: "Volume"}]
        }
    }

    #[inline] pub fn params(&self, tab_id: usize, self_id: usize) -> Html {
        match *self {
            Sound::Note{note, len, attack, release, ..} => match tab_id {
                0 /* General */ => html!{<div id="inputs">
                    <Slider key="note-dur"
                    id={ParamId::Duration(self_id)}
                    max={r64![100.0]}
                    name="Note Duration" postfix="Beats"
                    initial={len}/>
                </div>},
                1 /* Pitch */ => html!{<div id="inputs">
                    <Switch key={format!("{self_id}-note")}
                    id={ParamId::Note(self_id)}
                    options={Note::NAMES.to_vec()}
                    name="Note"
                    initial={note.index()}/>
                    <Slider key={format!("{self_id}-att")}
                    id={ParamId::Attack(self_id)}
                    max={r64![5.0]}
                    name="Attack Time" postfix="Beats"
                    initial={attack}/>
                    <Slider key={format!("{self_id}-rel")}
                    id={ParamId::Release(self_id)}
                    max={r64![5.0]}
                    name="Release Time" postfix="Beats"
                    initial={release}/>
                </div>},
                tab_id => html!{<p style="color:red">{format!("Invalid tab ID: {tab_id}")}</p>}
            }


            Sound::Noise{len, ref gain, ..} => match tab_id {
                0 /* General */ => html!{<div id="inputs">
                    <Slider key="noise-dur"
                    id={ParamId::Duration(self_id)}
                    max={r64![100.0]}
                    name="Noise Duration" postfix="Beats"
                    initial={len}/>
                </div>},
                1 /* Volume */ => html!{<div id="inputs">
                    <Slider key={format!("{self_id}-noise-vol")}
                    id={ParamId::Volume(self_id)}
                    name="Noise Volume"
                    initial={R64::new_or(R64::ZERO, gain.gain().value() as f64)}/>
                </div>},
                tab_id => html!{<p style="color:red">{format!("Invalid tab ID: {tab_id}")}</p>}
            }
        }
    }

    pub fn set_param(&mut self, id: ParamId, value: R64) -> bool {
        match self {
            Sound::Note{note, len, attack, release, ..} => match id {
                ParamId::Note(_) =>
                    *note = Note::from_index(*value as usize),
                ParamId::Duration(_) =>
                    *len = value,
                ParamId::Attack(_) =>
                    *attack = value,
                ParamId::Release(_) =>
                    *release = value,
                _ => ()
            }.pipe(|_| false),

            Sound::Noise{len, gain, ..} => match id {
                ParamId::Duration(_) =>
                    *len = value,
                ParamId::Volume(_) =>
                    gain.gain().set_value(*value as f32),
                _ => (),
            }.pipe(|_| false)
        }
    }
}
