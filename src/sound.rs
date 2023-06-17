use std::{
    ops::{Add, Sub, Neg, Range},
    fmt::{self, Display, Formatter, Debug},
    f64::consts::PI,
    mem::transmute};
use js_sys::Math::random;
use wasm_bindgen::{JsValue, JsCast};
use web_sys::{
    AudioNode,
    AudioContext,
    OscillatorNode,
    AudioBufferSourceNode,
    AudioBuffer,
    GainNode,
    Path2d};
use yew::{html, Html, Callback};
use crate::{
    utils::{
        JsResult,
        JsResultUtils,
        R64, R32,
        SaturatingInto, Pipe, RatioToInt, LooseEq},
    input::{ParamId, Slider},
    visual::{GraphEditor, Graphable, CanvasEvent},
    loc,
    r32, r64,
};

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
pub struct PitchPoint {
    pub offset: Beats,
    pub value: Note
}

impl Graphable for PitchPoint {
    const EDITOR_NAME: &'static str = "Pitch Editor";
    const SCALE_X_BOUND: Range<R32> = r32![3.0] .. r32![30.0];
    const SCALE_Y_BOUND: Range<R32> = r32![5.0] .. r32![36.0];
    type Inner = ();
    type Event = ();

    #[inline] fn inner(&self) -> &Self::Inner {&()}
    #[inline] fn inner_mut(&mut self) -> &mut Self::Inner {
        unsafe{transmute(self)}
    }

    #[inline] fn loc(&self) -> [R32; 2] {
        [self.offset.into(), self.value.index().into()]
    }

    fn draw(&self, next: Option<&Self>, mapper: impl Fn([f64; 2]) -> [f64; 2]) -> JsResult<Path2d> {
        let res = Path2d::new().add_loc(loc!())?;
        let src = mapper([*self.offset, self.value.index() as f64]);
        res.ellipse(src[0], src[1], 5.0, 5.0, 0.0, 0.0, PI * 2.0).add_loc(loc!())?;
        if let Some(next) = next {
            let dst = mapper([*next.offset, next.value.index() as f64]);
            res.move_to(src[0], src[1]);
            res.line_to(dst[0], dst[1]);
        }
        Ok(res)
    }

    #[inline] fn set_loc(&mut self, n_points: usize, self_id: usize, x: impl FnOnce() -> R32, y: impl FnOnce() -> R32) {
        if self_id != n_points - 2 {
            self.offset = x().into();
        }
        self.value = Note::from_index(y().to_int());
    }

    #[inline] fn in_hitbox(&self, point: [R32; 2]) -> bool {
        self.value.index() == *point[1] as usize
            && R32::from(self.offset).loose_eq(point[0], 0.1)
    }

    #[inline] fn fmt_loc(loc: [R32; 2]) -> String {
        format!("{:.3}, {}", *loc[0], Note::from_index(loc[1].to_int()))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sound {
    Note{gen: OscillatorNode, state: usize, pitch: GraphEditor<PitchPoint>},
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
                Self::Note{gen, state: 0,
                    pitch: GraphEditor::new(r32![5.0], r32![20.0])}
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

    pub fn reset(&mut self, ctx: &AudioContext) -> JsResult<()> {
        Ok(match self {
            Sound::Note{state, gen, pitch, ..} => {
                *state = 0;
                let first = unsafe{pitch.get_unchecked(0)};
                gen.frequency().cancel_scheduled_values(0.0).add_loc(loc!())?
                    .set_value(*first.value.freq());
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

    pub fn poll(&mut self, mut time: Secs, plug: &AudioNode, bps: Beats) -> JsResult<Secs> {
        Ok(match self {
            Sound::Note{gen, state, pitch, ..} => {
                let cur = unsafe{pitch.data().get_unchecked(*state)};
                *state += 1;
                if let Some(next) = pitch.get(*state) {
                    time += *(next.offset - cur.offset).to_secs(bps);
                    gen.frequency()
                        .linear_ramp_to_value_at_time(*cur.value.freq(), *time)
                        .add_loc(loc!())?;
                    time
                } else {
                    gen.disconnect().add_loc(loc!())?;
                    Secs::INFINITY
                }
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

    /// given guarantee: always stop the sound properly regardless of whether an error is returned
    /// expected guarantee: called in the same manner as `poll` would be called
    pub fn stop(&mut self, mut time: Secs, bps: Beats) -> JsResult<Secs> {
        match self {
            Sound::Note{gen, pitch, state, ..} => match (pitch.data(), pitch.data().len() - *state) {
                ([.., start, end], 2..) => {
                    time += (end.offset - start.offset).to_secs(bps);
                    gen.frequency()
                        .linear_ramp_to_value_at_time(*end.value.freq(), *time)
                        .add_loc(loc!()).map(|_| time)
                }

                _ => gen.disconnect().add_loc(loc!()).map(|_| Beats::INFINITY)
            }

            Sound::Noise{gen, ..} => gen.disconnect().add_loc(loc!())
                .map(|_| Beats::INFINITY)
        }
    }

    #[inline] pub fn len(&self) -> Beats {
        match self {
            Sound::Note{pitch, ..} =>
                pitch.data().last().map_or(r64!{0.0}, |x| x.offset),
            Sound::Noise{len, ..} => *len
        }
    }

    #[inline] pub fn tabs(&self) -> &'static [TabInfo] {
        match self {
            Sound::Note{..} =>
                &[],
            Sound::Noise{..} =>
                &[TabInfo{name: "General"}, TabInfo{name: "Volume"}]
        }
    }

    #[inline] pub fn params(&self, tab_id: usize, self_id: usize, _param_setter: Callback<(ParamId, R64)>) -> Html {
        match self {
            Sound::Note{pitch, ..} => html!{
                <canvas ref={pitch.canvas().clone()}/>
            },

            Sound::Noise{len, gain, ..} => match tab_id {
                0 /* General */ => html!{<div id="inputs">
                    <Slider key="noise-dur"
                    id={ParamId::Duration(self_id)}
                    max={r64![100.0]}
                    name="Noise Duration" postfix="Beats"
                    initial={*len}/>
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

    pub fn set_param(&mut self, id: ParamId, value: R64) -> JsResult<bool> {
        Ok(match self {
            Sound::Note{pitch, ..} => match id {
                ParamId::HoverTab(_, e) => {
                    let e = CanvasEvent::try_from(&*e).add_loc(loc!())?;
                    pitch.handle_hover(Some(e));
                }

                ParamId::LeavePlane => _ = pitch.handle_hover(None),

                ParamId::Resize => pitch.handle_resize(),

                _ => ()
            }.pipe(|_| false),

            Sound::Noise{len, gain, ..} => match id {
                ParamId::Duration(_) =>
                    *len = value,
                ParamId::Volume(_) =>
                    gain.gain().set_value(*value as f32),
                _ => (),
            }.pipe(|_| false)
        })
    }
}
