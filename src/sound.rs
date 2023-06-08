use std::{
    ops::{Add, Sub, Neg},
    fmt::{self, Display, Formatter, Debug}, rc::Rc};
use web_sys::{
    AudioNode,
    AudioContext,
    OscillatorNode, OscillatorOptions};
use yew::{html, Html};
use crate::{
    utils::{
        JsResult,
        JsResultUtils,
        SliceExt,
        R64, R32,
        SaturatingInto, Pipe, BoolExt},
    input::{Switch, ParamId, Slider},
    loc,
    r32, r64, visual::HintHandler};

pub type MSecs = R64;
pub type Secs = R64;
pub type Beats = R64;

pub trait FromBeats {
    fn to_msecs(self, bps: Self) -> MSecs;
    fn to_secs(self, bps: Self) -> Secs;
}

impl FromBeats for Beats {
    #[inline]
    fn to_secs(self, bps: Self) -> Secs {self / bps}

    #[inline]
    fn to_msecs(self, bps: Self) -> MSecs {self / bps * r64![1000.0]}
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

    pub fn from_freq(value: R32) -> Self {
        const MAX: usize = Note::FREQS.len() - 1;
        Self(Self::FREQS.iter()
            .position(|&freq| value <= freq)
            .unwrap_or(MAX) as u8)
    }

    #[inline] pub const fn name(&self) -> &'static str {
        unsafe{Self::NAMES.get_unchecked(self.0 as usize)}
    }

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SoundType {
    Note
}

impl SoundType {
    #[inline] pub fn name(&self) -> &'static str {
        match self {
            SoundType::Note => "Note"
        }
    }

    #[inline] pub fn init(&self, ctx: &AudioContext) -> JsResult<Sound> {
        match self {
            SoundType::Note => Sound::new_note(ctx)
        }
    }

    pub fn desc(&self, offset: Beats, layer: i32) -> String {
        format!("{} @{:.3}, layer {}", self.name(), *offset, layer)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Sound {
    Note{note: Note, len: Beats, gen: OscillatorNode, started: bool}
}

impl Sound {
    pub const TYPES: [SoundType; 1] = [
        SoundType::Note
    ];

    #[inline] fn new_note(ctx: &AudioContext) -> JsResult<Self> {
        let gen = OscillatorNode::new(ctx).add_loc(loc!())?;
        Ok(Self::Note{note: Note::MAX, len: r64![1.0],
            gen, started: false})
    }

    #[inline] pub fn name(&self) -> &'static str {
        match self {
            Sound::Note{..} => "Note"
        }
    }

    #[inline] pub fn reset(&mut self, ctx: &AudioContext) -> JsResult<()> {
        Ok(match self {
            Sound::Note{gen, note, ..} => {
                *gen = OscillatorNode::new_with_options(ctx,
                    OscillatorOptions::new().frequency(*note.freq())).add_loc(loc!())?
            }
        })
    }

    #[inline] pub fn poll(&mut self, time: Secs, plug: &AudioNode, bps: Beats) -> JsResult<Secs> {
        Ok(match self {
            Sound::Note{len, gen, started, ..} => if started.toggle() {
                self.stop(time).add_loc(loc!())?;
                Secs::INFINITY
            } else {
                gen.connect_with_audio_node(plug).add_loc(loc!())?;
                gen.start().add_loc(loc!())?;
                len.to_secs(bps) + time
            }
        })
    }

    #[inline] pub fn stop(&mut self, time: Secs) -> JsResult<()> {
        Ok(match self {
            Sound::Note{gen, ..} => {
                gen.frequency()
                    .exponential_ramp_to_value_at_time(f32::MIN_POSITIVE, *time + 0.2).add_loc(loc!())?;
            }
        })
    }

    #[inline] pub fn len(&self) -> Beats {
        match self {
            &Sound::Note{len, ..} => len
        }
    }

    #[inline] pub fn params(&self, id: usize, hint: &Rc<HintHandler>) -> Html {
        match self {
            &Sound::Note{note, len, ..} => html!{<div id="inputs">
                <Switch {hint}
                key="note"
                id={ParamId::Note(id)}
                options={Note::NAMES.to_vec()}
                name="Note"
                initial={note.index()}/>
                <Slider {hint}
                key="note-len"
                id={ParamId::NoteLength(id)}
                max={r64![100.0]}
                name="Note Length" postfix="Beats"
                initial={len}/>
            </div>}
        }
    }

    pub fn set_param(&mut self, id: ParamId, value: R64) -> bool {
        match self {
            Sound::Note{note, len, ..} => match id {
                ParamId::Note(_) =>
                    *note = Note::from_index(*value as usize),
                ParamId::NoteLength(_) =>
                    *len = value,
                _ => ()
            }.pipe(|_| false)
        }
    }
}
