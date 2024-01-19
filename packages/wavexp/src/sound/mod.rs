mod note;
use note::*;
mod noise;
use noise::*;
mod custom;
use custom::*;

use crate::{
    ctx::{AppEvent, ContextMut, ContextRef, EditorAction},
    input::Button,
    sequencer::Sequencer,
};
use std::{
    fmt::{self, Display, Formatter},
    future::Future,
    mem::{replace, variant_count},
    num::NonZeroUsize,
    ops::{Add, AddAssign, Div, Sub, SubAssign},
    rc::Rc,
};
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::JsFuture;
use wavexp_utils::{
    default,
    ext::{BoolExt, OptionExt},
    r32, r64, AppResult, AppResultUtils, R32, R64,
};
use web_sys::{AudioBuffer, AudioBufferOptions, AudioNode, BaseAudioContext, File};
use yew::{html, Html};

pub type MSecs = R64;
pub type Secs = R64;
pub type Beats = R64;

pub trait FromBeats {
    fn to_msecs(self, bps: Self) -> MSecs;
    fn to_secs(self, bps: Self) -> Secs;
    fn secs_to_beats(self, bps: Self) -> Beats;
}

impl FromBeats for Beats {
    fn to_secs(self, bps: Self) -> Secs {
        self / bps
    }
    fn to_msecs(self, bps: Self) -> MSecs {
        self * 1000u16 / bps
    }
    fn secs_to_beats(self, bps: Self) -> Beats {
        self * bps
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, PartialOrd, Ord)]
// Safety invariant: `self.0 <= Self::MAX.0`
pub struct Note(u8);

impl Display for Note {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(unsafe { Self::NAMES.get_unchecked(self.0 as usize) }, f)
    }
}

impl Add<isize> for Note {
    type Output = Note;
    fn add(self, rhs: isize) -> Self::Output {
        self.checked_add(rhs)
            .to_app_result()
            .report()
            .unwrap_or(self)
    }
}

impl AddAssign<isize> for Note {
    fn add_assign(&mut self, rhs: isize) {
        self.checked_add_assign(rhs).to_app_result().report();
    }
}

impl Sub<isize> for Note {
    type Output = Note;
    fn sub(self, rhs: isize) -> Self::Output {
        self.checked_sub(rhs)
            .to_app_result()
            .report()
            .unwrap_or(self)
    }
}

// TODO: remove the side effects by introducing `Try{Add/Sub/Mul/Div}` traits
impl Sub for Note {
    type Output = isize;
    fn sub(self, rhs: Self) -> Self::Output {
        let x = self.0 as isize;
        x.checked_sub(rhs.0 as isize)
            .to_app_result()
            .report()
            .unwrap_or(x)
    }
}

impl SubAssign<isize> for Note {
    fn sub_assign(&mut self, rhs: isize) {
        self.checked_sub_assign(rhs).to_app_result().report();
    }
}

impl Note {
    pub const MAX: Note = Note(35);
    pub const MID: Note = Note(Self::N_NOTES as u8 / 2);
    pub const N_NOTES: usize = Self::FREQS.len();
    pub const FREQS: [R32; 36] = [
        r32![65.410], /*C2*/
        r32![69.300], /*C#2*/
        r32![73.420], /*D2*/
        r32![77.780], /*D#2*/
        r32![82.410], /*E2*/
        r32![87.310], /*F2*/
        r32![92.500], /*F#2*/
        r32![98.000], /*G2*/
        r32![103.83], /*G#2*/
        r32![110.00], /*A2*/
        r32![116.54], /*A#2*/
        r32![123.47], /*B2*/
        r32![130.81], /*C3*/
        r32![138.59], /*C#3*/
        r32![146.83], /*D3*/
        r32![155.56], /*D#3*/
        r32![164.81], /*E3*/
        r32![174.61], /*F3*/
        r32![185.00], /*F#3*/
        r32![196.00], /*G3*/
        r32![207.65], /*G#3*/
        r32![220.00], /*A3*/
        r32![233.08], /*A#3*/
        r32![246.94], /*B3*/
        r32![261.63], /*C4*/
        r32![277.18], /*C#4*/
        r32![293.66], /*D4*/
        r32![311.13], /*D#4*/
        r32![329.63], /*E4*/
        r32![349.23], /*F4*/
        r32![369.99], /*F#4*/
        r32![392.00], /*G4*/
        r32![415.30], /*G#4*/
        r32![440.00], /*A4*/
        r32![466.16], /*A#4*/
        r32![493.88], /*B4*/
    ];

    pub const NAMES: [&'static str; 36] = [
        "C2", "C#2", "D2", "D#2", "E2", "F2", "F#2", "G2", "G#2", "A2", "A#2", "B2", "C3", "C#3",
        "D3", "D#3", "E3", "F3", "F#3", "G3", "G#3", "A3", "A#3", "B3", "C4", "C#4", "D4", "D#4",
        "E4", "F4", "F#4", "G4", "G#4", "A4", "A#4", "B4",
    ];

    pub fn checked_add(self, rhs: isize) -> Option<Self> {
        let new = self.0 as isize + rhs;
        if new >= 0 && new < Self::N_NOTES as isize {
            Some(Self(new as u8))
        } else {
            None
        }
    }

    pub fn checked_add_assign(&mut self, rhs: isize) -> bool {
        if let Some(x) = self.checked_add(rhs) {
            *self = x;
            true
        } else {
            false
        }
    }

    pub fn checked_sub(self, rhs: isize) -> Option<Self> {
        let new = self.0 as isize - rhs;
        if new >= 0 && new < Self::N_NOTES as isize {
            Some(Self(new as u8))
        } else {
            None
        }
    }

    pub fn checked_sub_assign(&mut self, rhs: isize) -> bool {
        if let Some(x) = self.checked_sub(rhs) {
            *self = x;
            true
        } else {
            false
        }
    }

    pub const fn from_index(value: usize) -> Self {
        if value >= Self::FREQS.len() {
            Self::MAX
        } else {
            Self(value as u8)
        }
    }

    pub const fn index(&self) -> usize {
        self.0 as usize
    }

    pub fn freq(&self) -> R32 {
        unsafe { *Self::FREQS.get_unchecked(self.0 as usize) }
    }

    pub const fn recip(self) -> Self {
        Self(Self::MAX.0 - self.0)
    }

    pub fn pitch_coef(&self) -> R64 {
        r64! {*self - Self::MID}.div(12u8).exp2()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct AudioInputChanges {
    /// Make the input play backwards.
    pub reversed: bool,
    /// cut the input from the start.
    pub cut_start: Beats,
    /// cut the input from the end.
    pub cut_end: Beats,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AudioInput {
    name: Rc<str>,
    duration: Secs,
    raw: AudioBuffer,
    raw_duration: Secs,
    pending_changes: AudioInputChanges,
    baked_changes: AudioInputChanges,
    baked: AudioBuffer,
}

impl AudioInput {
    pub fn new_file(file: File, sequencer: &Sequencer) -> impl Future<Output = AppResult<Self>> {
        Self::new_file_base(file, sequencer.audio_ctx().clone())
    }

    async fn new_file_base(file: File, audio_ctx: BaseAudioContext) -> AppResult<Self> {
        let raw = JsFuture::from(file.array_buffer()).await?.dyn_into()?;
        let mut raw: AudioBuffer = JsFuture::from(audio_ctx.decode_audio_data(&raw)?)
            .await?
            .dyn_into()?;
        if raw.number_of_channels() < Sequencer::CHANNEL_COUNT {
            let new_raw = AudioBuffer::new(
                AudioBufferOptions::new(raw.length(), Sequencer::SAMPLE_RATE as f32)
                    .number_of_channels(Sequencer::CHANNEL_COUNT),
            )?;
            let data = raw.get_channel_data(0)?;
            for i in 0..Sequencer::CHANNEL_COUNT as i32 {
                new_raw.copy_to_channel(&data, i)?;
            }
            raw = new_raw;
        }
        let name = format!("File {:?}", file.name()).into();
        let duration = R64::try_from(raw.duration())?;
        Ok(Self {
            name,
            baked: raw.clone(),
            raw,
            duration,
            raw_duration: duration,
            pending_changes: default(),
            baked_changes: default(),
        })
    }

    /// Name of the input, exists solely for the user's convenience.
    pub fn name(&self) -> &Rc<str> {
        &self.name
    }
    /// Sets the name of the input, returning the old one.
    pub fn set_name(&mut self, name: Rc<str>) -> Rc<str> {
        replace(&mut self.name, name)
    }
    /// Duration of the raw buffer, unchanged since the moment the input was created.
    pub fn raw_duration(&self) -> Secs {
        self.raw_duration
    }
    /// Duration of the buffer with all the requested changes baked in.
    pub fn baked_duration(&self) -> Secs {
        self.duration
    }

    // /// Raw buffer, unchanged since the moment the input was created.
    // pub fn raw(&self) -> &AudioBuffer {&self.raw}

    /// Get a struct holding all the changes yet to be baked into the input.
    pub fn changes(&self) -> AudioInputChanges {
        self.pending_changes
    }
    /// Get a mutable reference to a struct holding all the changes yet to be baked into the input.
    pub fn changes_mut(&mut self) -> &mut AudioInputChanges {
        &mut self.pending_changes
    }

    /// Bake all of the changes into a buffer that will be accessible through `.baked()` method.
    /// If an error occurs, the input will appear unbaked.
    pub fn bake(&mut self, bps: Beats) -> AppResult<()> {
        if self.pending_changes == self.baked_changes {
            return Ok(());
        };
        let cut_start =
            (*self.pending_changes.cut_start.to_secs(bps) * Sequencer::SAMPLE_RATE as f64) as usize;
        let cut_end =
            (*self.pending_changes.cut_end.to_secs(bps) * Sequencer::SAMPLE_RATE as f64) as usize;
        let length = self.raw.length() - cut_start as u32 - cut_end as u32;
        self.baked = AudioBuffer::new(
            AudioBufferOptions::new(length, Sequencer::SAMPLE_RATE as f32)
                .number_of_channels(Sequencer::CHANNEL_COUNT),
        )?;

        // TODO: this doesn't affect anything for some reason.
        self.duration = R64::from(length) / Sequencer::SAMPLE_RATE;
        for i in 0..Sequencer::CHANNEL_COUNT {
            let mut data = self.raw.get_channel_data(i)?;
            if self.pending_changes.reversed {
                data.reverse();
            }
            self.baked.copy_to_channel(&data[cut_start..], i as i32)?;
        }

        Ok(self.baked_changes = self.pending_changes)
    }

    /// Buffer with all the requested changes baked in.
    /// If the there are unbaked changes, `None` is returned.
    pub fn baked(&self) -> Option<&AudioBuffer> {
        (self.pending_changes == self.baked_changes).then_some(&self.baked)
    }

    pub fn desc(&self, bps: Beats) -> String {
        format!(
            "{}, {:.2} beats",
            self.name,
            self.duration.secs_to_beats(bps)
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SoundType {
    Note,
    Noise,
    Custom,
}

impl SoundType {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Note => NoteSound::NAME,
            Self::Noise => NoiseSound::NAME,
            Self::Custom => CustomSound::NAME,
        }
    }
}

#[derive(Default, Debug, Clone)]
pub enum Sound {
    #[default]
    None,
    Note(NoteSound),
    Noise(NoiseSound),
    Custom(CustomSound),
}

impl Sound {
    pub const TYPES: [SoundType; variant_count::<Self>() - 1 /* None */] = [
        SoundType::Note,
        SoundType::Noise,
        SoundType::Custom
    ];

    pub fn new(sound_type: SoundType) -> AppResult<Self> {
        Ok(match sound_type {
            SoundType::Note => Self::Note(default()),
            SoundType::Noise => Self::Noise(NoiseSound::new()?),
            SoundType::Custom => Self::Custom(default()),
        })
    }

    pub fn name(&self) -> &'static str {
        match self {
            Self::None => "Undefined",
            Self::Note(_) => NoteSound::NAME,
            Self::Noise(_) => NoiseSound::NAME,
            Self::Custom(_) => CustomSound::NAME,
        }
    }

    pub fn prepare(&mut self, bps: Beats) -> AppResult<()> {
        match self {
            Sound::None => Ok(()),
            Sound::Note(inner) => inner.prepare(),
            Sound::Noise(inner) => inner.prepare(),
            Sound::Custom(inner) => inner.prepare(bps),
        }
    }

    pub fn play(
        &self,
        plug: &AudioNode,
        now: Secs,
        self_offset: Secs,
        bps: Beats,
    ) -> AppResult<()> {
        match self {
            Self::None => Ok(()),
            Self::Note(inner) => inner.play(plug, now, self_offset, bps),
            Self::Noise(inner) => inner.play(plug, now, self_offset, bps),
            Self::Custom(inner) => inner.play(plug, now, self_offset, bps),
        }
    }

    pub fn len(&self, bps: Beats) -> AppResult<Beats> {
        match self {
            Self::None => Ok(r64![1]),
            Self::Note(inner) => inner.len(),
            Self::Noise(inner) => inner.len(),
            Self::Custom(inner) => inner.len(bps),
        }
    }

    pub fn rep_count(&self) -> NonZeroUsize {
        match self {
            Self::None => NonZeroUsize::MIN,
            Self::Note(inner) => inner.rep_count(),
            Self::Noise(inner) => inner.rep_count(),
            Self::Custom(inner) => inner.rep_count(),
        }
    }

    pub fn params(&self, ctx: ContextRef, sequencer: &Sequencer) -> Html {
        match self {
            Self::None => {
                let emitter = ctx.event_emitter();
                html! {<div class="horizontal-menu">
                    {for Sound::TYPES.iter().map(|x| html!{
                        <Button name={x.name()}
                            onclick={emitter.reform(|_| AppEvent::SetBlockType(*x))}>
                            <p>{x.name()}</p>
                        </Button>
                    })}
                </div>}
            }

            Self::Note(inner) => inner.params(ctx),
            Self::Noise(inner) => inner.params(ctx),
            Self::Custom(inner) => inner.params(ctx, sequencer),
        }
    }

    pub fn handle_event(
        &mut self,
        event: &AppEvent,
        mut ctx: ContextMut,
        sequencer: &Sequencer,
        offset: Beats,
    ) -> AppResult<()> {
        let r = &mut false;
        match self {
            Sound::None => match event {
                &AppEvent::SetBlockType(ty) => {
                    *self = Self::new(ty)?;
                    ctx.register_action(EditorAction::SetBlockType(ty))?;
                    ctx.emit_event(AppEvent::RedrawEditorPlane);
                }

                AppEvent::Redo(actions) => {
                    for action in actions.iter() {
                        if let &EditorAction::SetBlockType(ty) = action {
                            *self = Self::new(ty)?;
                            ctx.emit_event(AppEvent::RedrawEditorPlane);
                        }
                    }
                }

                _ => (),
            },

            Sound::Note(inner) => inner.handle_event(event, ctx, sequencer, r, offset)?,
            Sound::Noise(inner) => inner.handle_event(event, ctx, sequencer, r, offset)?,
            Sound::Custom(inner) => inner.handle_event(event, ctx, sequencer, r, offset)?,
        };
        if *r {
            *self = Self::None
        }
        Ok(())
    }
}
