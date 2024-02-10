//! defines decoding/encoding of a composition

use crate::sequencer::{Composition, Sequencer};
use crate::sound::FromBeats;
use crate::{
    sequencer::SoundBlock,
    sound::{
        AudioInput, CustomBlock, CustomSound, NoiseBlock, NoiseSound, Note, NoteBlock, NoteSound,
        Sound,
    },
    visual::{GraphEditor, GraphPoint},
};
use hound::{SampleFormat, WavSpec, WavWriter};
use js_sys::ArrayBuffer;
use std::future::Future;
use std::io::Cursor;
use std::iter::zip;
use std::ops::{Add, Mul};
use std::{
    num::{
        NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI8, NonZeroU16, NonZeroU32, NonZeroU64,
        NonZeroU8,
    },
    rc::Rc,
    slice::from_raw_parts,
    str::from_utf8,
};
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::JsFuture;
use wavexp_utils::ext::default;
use wavexp_utils::{
    bail,
    cell::Shared,
    ensure,
    error::Result,
    ext::{BoolExt, SliceExt},
    real::{R32, R64},
    TryÍnto,
};
use wavexp_utils::{const_assert, r64};
use web_sys::{AudioBuffer, AudioBufferOptions, BaseAudioContext, OfflineAudioContext};

impl Composition {
    const WAVEXP_HEADER: [u8; 8] = *b"3XPL0RE!";

    /// decodes the contents of a `.wavexp` file
    pub fn decode(src: &mut &[u8]) -> Result<Self> {
        let header: [u8; 8] = decode(src)?;
        ensure!(header == Self::WAVEXP_HEADER, "invalid header");
        Ok(Self {
            pattern: decode(src)?,
            inputs: decode_short(src)?,
            bps: decode(src)?,
        })
    }

    /// imports extenal audio and creates a composition of 1 custom audio block
    pub async fn import(
        src_name: Rc<str>,
        src: &ArrayBuffer,
        ctx: &BaseAudioContext,
    ) -> Result<Self> {
        let src = JsFuture::from(ctx.decode_audio_data(src)?)
            .await?
            .unchecked_into::<AudioBuffer>();
        let src = Shared::from(AudioInput::new(src_name, src)?);
        Ok(Self {
            pattern: Shared::from(GraphEditor::new(vec![SoundBlock {
                sound: Sound::Custom(CustomSound {
                    pattern: Shared::from(GraphEditor::new(vec![CustomBlock {
                        offset: r64!(0),
                        pitch: Note::MID,
                    }])),
                    src: Some(src.clone()),
                    ..default()
                }),
                layer: 0,
                offset: r64!(0),
            }])),
            inputs: vec![src],
            ..default()
        })
    }

    /// encode the composition into the `.wavexp` file format
    pub fn encode(&self) -> Result<Vec<u8>> {
        let mut dst = vec![];
        dst.extend(Self::WAVEXP_HEADER);
        self.pattern.encode(&mut dst)?;
        self.inputs.encode_short(&mut dst)?;
        self.bps.encode(&mut dst)?;
        Ok(dst)
    }

    /// export the composition into the `.wav` audio file format with the provided volume
    pub fn export(&self, volume: R32) -> Result<impl Future<Output = Result<Vec<u8>>>> {
        let mut pat = self.pattern.get_mut()?;
        let renderer = OfflineAudioContext::new_with_number_of_channels_and_length_and_sample_rate(
            Sequencer::CHANNEL_COUNT,
            'len: {
                let Some(last) = pat.data().last() else {
                    break 'len 1;
                };
                last.len(self.bps)?
                    .add(last.offset)
                    .mul(Sequencer::SAMPLE_RATE)
                    .max(r64!(1))
                    .into()
            },
            Sequencer::SAMPLE_RATE as f32,
        )?;
        let gain = renderer.create_gain()?;
        gain.gain().set_value(*volume);
        gain.connect_with_audio_node(&renderer.destination())?;
        for mut block in pat.iter_data_mut() {
            block.inner().prepare(self.bps)?;
        }
        for mut block in pat.iter_data_mut() {
            let offset = block.offset.to_secs(self.bps);
            block.inner().play(&gain, R64::ZERO, offset, self.bps)?;
        }

        Ok(async move {
            let rendered = JsFuture::from(renderer.start_rendering()?)
                .await?
                .unchecked_into::<AudioBuffer>();
            let mut wav: Cursor<Vec<u8>> = default();
            let mut wav_writer = WavWriter::new(
                &mut wav,
                WavSpec {
                    channels: Sequencer::CHANNEL_COUNT as u16,
                    sample_rate: Sequencer::SAMPLE_RATE,
                    bits_per_sample: 32,
                    sample_format: SampleFormat::Float,
                },
            )?;

            const_assert!(Sequencer::CHANNEL_COUNT == 2);
            let ch1 = rendered.get_channel_data(0)?;
            let ch2 = rendered.get_channel_data(1)?;
            for (s1, s2) in zip(ch1, ch2) {
                wav_writer.write_sample(s1)?;
                wav_writer.write_sample(s2)?;
            }
            wav_writer.finalize()?;
            Ok(wav.into_inner())
        })
    }
}

// TODO: make propagated errors more informative by adding an API to `AppError` for nesting error
// context
// or maybe not idk really what do I do with the errors and how much info is really needed, the
// call stack will be there regardless

fn decode<T: Persist>(src: &mut &[u8]) -> Result<T> {
    T::decode(src)
}

fn decode_short<T: PersistShort>(src: &mut &[u8]) -> Result<T> {
    T::decode_short(src)
}

fn decode_bytes<'src>(src: &mut &'src [u8], n: usize) -> Result<&'src [u8]> {
    let Some((res, rest)) = src.try_split_at(n) else {
        bail!("unexpected EOF while decoding a sequence of {n} bytes")
    };
    *src = rest;
    Ok(res)
}

fn decode_f32_seq<'src>(src: &mut &'src [u8], n: usize) -> Result<&'src [f32]> {
    ensure!(let Some(n_bytes) = n.checked_mul(4), "32-bit float sequence too long");
    let bytes = decode_bytes(src, n_bytes)?;
    Ok(unsafe { from_raw_parts(bytes.as_ptr().cast(), n) })
}

fn encode_f32_seq(seq: &[f32], dst: &mut Vec<u8>) {
    dst.extend(unsafe { from_raw_parts(seq.as_ptr().cast(), seq.len()) })
}

trait Persist: Sized {
    fn decode(src: &mut &[u8]) -> Result<Self>;
    fn encode(&self, dst: &mut Vec<u8>) -> Result;
}

/// Different from [`Persist`] to allow some types to have 2 kinds of encodings: one normal and one
/// shorter but that comes with some assumptions and is more niche.
/// A short integer is an integer stored as chunks of 255 + a trailing byte with a value less
/// than 255, which serves as a terminator for the sequence.
/// Examples of flat representation:
/// - 0x86  - `[0x86]`
/// - 0x100 - `[0xFF, 0x01]`
/// - 0x200 - `[0xFF, 0xFF, 0x01]`
/// A short float is confined to the range 0 ..= 1 in a saturating manner and stored as 1 byte
trait PersistShort: Sized {
    fn decode_short(src: &mut &[u8]) -> Result<Self>;
    fn encode_short(&self, dst: &mut Vec<u8>) -> Result;
}

macro_rules! impl_persist_for_ints {
    ($($int:ty),+) => {
        $(
            impl Persist for $int {
                #[inline]
                fn decode(src: &mut &[u8]) -> Result<Self> {
                    decode(src).map(Self::from_le_bytes)
                }

                #[inline]
                fn encode(&self, dst: &mut Vec<u8>) -> Result {
                    Ok(dst.extend(self.to_le_bytes()))
                }
            }
        )+
    };
}

// not isize/uzize as its layout is platform-dependent
impl_persist_for_ints!(i8, u8, i16, u16, i32, u32, i64, u64);

macro_rules! impl_persist_for_nonzeros {
    ($($nonzero:ty : $int:ty ),+ $(,)?) => {
        $(
            impl Persist for $nonzero {
                fn decode(src: &mut &[u8]) -> Result<Self> {
                    Ok(<$int>::decode(src)?.try_into()?)
                }

                #[inline]
                fn encode(&self, dst: &mut Vec<u8>) -> Result {
                    self.get().encode(dst)
                }
            }
        )+
    };
}

impl_persist_for_nonzeros!(
    NonZeroI8:  i8,  NonZeroU8:  u8,
    NonZeroI16: i16, NonZeroU16: u16,
    NonZeroI32: i32, NonZeroU32: u32,
    NonZeroI64: i64, NonZeroU64: u64,
);

macro_rules! impl_persist_for_reals {
    ($($real:ty : $float:ty),+) => {
        $(
            impl Persist for $real {
                fn decode(src: &mut &[u8]) -> Result<Self> {
                    <$float>::from_le_bytes(decode(src)?).try_into()
                }

                #[inline]
                fn encode(&self, dst: &mut Vec<u8>) -> Result {
                    Ok(dst.extend(self.to_le_bytes()))
                }
            }
        )+
    }
}

impl_persist_for_reals!(R32: f32, R64: f64);

macro_rules! impl_persist_for_short_ints {
    ($($int:ty),*) => {
        $(
            impl PersistShort for $int {
                fn decode_short(src: &mut &[u8]) -> Result<Self> {
                    let mut res: $int = 0;
                    let mut iter = src.iter();
                    let mut ended_gracefully = false;
                    for &byte in &mut iter {
                        let Some(added) = res.checked_add(byte as $int) else {
                            bail!("short {} overflow", stringify!($int))
                        };
                        res = added;
                        if byte != 0xFF {
                            ended_gracefully = true;
                            break
                        };
                    }
                    if !ended_gracefully {
                        bail!("unexpected EOF while decoding a flat {}", stringify!($int))
                    }
                    *src = iter.as_slice();
                    Ok(res)
                }

                fn encode_short(&self, dst: &mut Vec<u8>) -> Result {
                    let n_full_bytes = usize::try_from(*self)? / 255;
                    dst.reserve(n_full_bytes + 1);
                    for _ in 0 .. n_full_bytes {
                        dst.push(0xFF);
                    }
                    Ok(dst.push(*self as u8))
                }
            }
        )+
    };
}

// not isize/uzize as its layout is platform-dependent
impl_persist_for_short_ints!(u16, u32, u64);

macro_rules! impl_persist_for_short_nonzeros {
    ($($nonzero:ty : $int:ty ),+ $(,)?) => {
        $(
            impl PersistShort for $nonzero {
                fn decode_short(src: &mut &[u8]) -> Result<Self> {
                    Ok(<$int>::decode_short(src)?.try_into()?)
                }

                #[inline]
                fn encode_short(&self, dst: &mut Vec<u8>) -> Result {
                    self.get().encode_short(dst)
                }
            }
        )+
    };
}

impl_persist_for_short_nonzeros!(NonZeroU16: u16, NonZeroU32: u32, NonZeroU64: u64);

macro_rules! impl_persist_for_short_reals {
    ($($real:ty),+) => {
        $(
            impl PersistShort for $real {
                fn decode_short(src: &mut &[u8]) -> Result<Self> {
                    Ok(<$real>::from(u8::decode(src)?) / u8::MAX)
                }

                #[inline]
                fn encode_short(&self, dst: &mut Vec<u8>) -> Result {
                    Ok(dst.push(u8::from(self * u8::MAX)))
                }
            }
        )+
    };
}

impl_persist_for_short_reals!(R32, R64);

impl<T: Persist> Persist for Option<T> {
    fn decode(src: &mut &[u8]) -> Result<Self> {
        (u8::decode(src)? != 0).then_try(|| decode(src))
    }

    fn encode(&self, dst: &mut Vec<u8>) -> Result {
        let Some(inner) = self else {
            return Ok(dst.push(0));
        };
        dst.push(1);
        inner.encode(dst)
    }
}

impl<const N: usize> Persist for [u8; N] {
    fn decode(src: &mut &[u8]) -> Result<Self> {
        let Some((&res, rest)) = src.split_first_chunk() else {
            bail!("unexpected EOF while decoding a chunk of {N} bytes")
        };
        *src = rest;
        Ok(res)
    }

    #[inline]
    fn encode(&self, dst: &mut Vec<u8>) -> Result {
        Ok(dst.extend(self))
    }
}

impl<T: Persist> PersistShort for Vec<T> {
    fn decode_short(src: &mut &[u8]) -> Result<Self> {
        let mut res = Vec::with_capacity(u32::decode_short(src)? as usize);
        for _ in 0..res.capacity() {
            res.push(T::decode(src)?)
        }
        Ok(res)
    }

    fn encode_short(&self, dst: &mut Vec<u8>) -> Result {
        self.len().try_ínto::<u32>()?.encode_short(dst)?;
        Ok(for item in self {
            item.encode(dst)?
        })
    }
}

impl PersistShort for Rc<str> {
    fn decode_short(src: &mut &[u8]) -> Result<Self> {
        let len: u32 = decode_short(src)?;
        let str = from_utf8(decode_bytes(src, len as usize)?)?;
        Ok(Rc::from(str))
    }

    fn encode_short(&self, dst: &mut Vec<u8>) -> Result {
        self.len().try_ínto::<u32>()?.encode_short(dst)?;
        dst.extend(self.as_bytes());
        Ok(())
    }
}

impl<T: Persist> Persist for Shared<T> {
    fn decode(src: &mut &[u8]) -> Result<Self> {
        T::decode(src).map(Self::from)
    }

    fn encode(&self, dst: &mut Vec<u8>) -> Result {
        self.get()?.encode(dst)
    }
}

impl<T: GraphPoint + Persist> Persist for GraphEditor<T> {
    fn decode(src: &mut &[u8]) -> Result<Self> {
        decode_short(src).map(Self::new)
    }

    fn encode(&self, dst: &mut Vec<u8>) -> Result {
        self.data().encode_short(dst)
    }
}

impl Persist for AudioBuffer {
    fn decode(src: &mut &[u8]) -> Result<Self> {
        let length = decode(src)?;
        let res = AudioBuffer::new(
            AudioBufferOptions::new(length, Sequencer::SAMPLE_RATE as f32)
                .number_of_channels(Sequencer::CHANNEL_COUNT),
        )?;
        for ch_id in 0..Sequencer::CHANNEL_COUNT as i32 {
            let ch = decode_f32_seq(src, length as usize)?;
            res.copy_to_channel(ch, ch_id)?;
        }
        Ok(res)
    }

    fn encode(&self, dst: &mut Vec<u8>) -> Result {
        self.length().encode(dst)?;
        for ch_id in 0..Sequencer::CHANNEL_COUNT {
            let ch = self.get_channel_data(ch_id)?;
            encode_f32_seq(&ch, dst);
        }
        Ok(())
    }
}

impl Persist for AudioInput {
    fn decode(src: &mut &[u8]) -> Result<Self> {
        let name = decode_short(src)?;
        let length = decode(src)?;
        let buffer = AudioBuffer::new(
            AudioBufferOptions::new(length, Sequencer::SAMPLE_RATE as f32)
                .number_of_channels(Sequencer::CHANNEL_COUNT),
        )?;
        for ch_id in 0..Sequencer::CHANNEL_COUNT as i32 {
            let ch = decode_f32_seq(src, length as usize)?;
            buffer.copy_to_channel(ch, ch_id)?;
        }
        AudioInput::new(name, buffer)
    }

    fn encode(&self, dst: &mut Vec<u8>) -> Result {
        let raw_buf = self.raw();

        self.name().encode_short(dst)?;
        raw_buf.length().encode(dst)?;
        for ch_id in 0..Sequencer::CHANNEL_COUNT {
            let ch = raw_buf.get_channel_data(ch_id)?;
            encode_f32_seq(&ch, dst);
        }
        Ok(())
    }
}

impl Persist for Note {
    fn decode(src: &mut &[u8]) -> Result<Self> {
        decode(src).map(Note::saturated)
    }

    fn encode(&self, dst: &mut Vec<u8>) -> Result {
        (**self).encode(dst)
    }
}

macro_rules! decoder {
    () => {
        decode
    };
    (short) => {
        decode_short
    };
}

macro_rules! encoder {
    () => {
        Persist::encode
    };
    (short) => {
        PersistShort::encode_short
    };
}

macro_rules! impl_persist_for_structs {
    ($( $struct:ty { $($field:ident $([ $mode:ident ])?),+ } ),+ $(,)?) => {
        $(
            impl Persist for $struct {
                fn decode(src: &mut &[u8]) -> Result<Self> {
                    Ok(Self {
                        $(
                            $field: decoder!($($mode)?)(src)?,
                        )+
                    })
                }

                fn encode(&self, dst: &mut Vec<u8>) -> Result {
                    $(
                        encoder!($($mode)?)(&self.$field, dst)?;
                    )+
                    Ok(())
                }
            }
        )+
    };
}

impl_persist_for_structs!(
    NoteBlock { offset, value, len },
    NoteSound { pattern, volume, attack, decay, sustain, release, rep_count },
    NoiseBlock { offset, pitch, len },
    NoiseSound { pattern, volume, attack, decay, sustain, release, rep_count },
    CustomBlock { offset, pitch },
    CustomSound { pattern, volume, attack, decay, sustain, release, rep_count, speed, src },
    SoundBlock { sound, layer[short], offset },
);

impl Persist for Sound {
    fn decode(src: &mut &[u8]) -> Result<Self> {
        match u8::decode(src)? {
            0 => Ok(Sound::None),
            1 => decode(src).map(Sound::Note),
            2 => decode(src).map(Sound::Noise),
            3 => decode(src).map(Sound::Custom),
            tag => bail!("invalid sound type tag: {tag}"),
        }
    }

    fn encode(&self, dst: &mut Vec<u8>) -> Result {
        match self {
            Sound::None => Ok(dst.push(0)),
            Sound::Note(note) => {
                dst.push(1);
                note.encode(dst)
            }
            Sound::Noise(noise) => {
                dst.push(2);
                noise.encode(dst)
            }
            Sound::Custom(custom) => {
                dst.push(3);
                custom.encode(dst)
            }
        }
    }
}
