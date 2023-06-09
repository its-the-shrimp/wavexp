use std::{cmp::Ordering, ops::Not};
use web_sys::{AnalyserNode as JsAnalyserNode, DynamicsCompressorNode, GainNode, AudioContext};
use crate::{
    sound::{Secs, Sound, Beats, FromBeats},
    utils::{JsResult, JsResultUtils, R64, VecExt, R32, OptionExt, ResultToJsResult},
    input::ParamId,
    loc, r64
};

#[derive(Debug)]
pub struct PatternBlock {
    pub sound: Sound,
    pub layer: i32,
    pub offset: Beats
}

impl PartialEq for PatternBlock {
    #[inline] fn eq(&self, other: &Self) -> bool {
        self.offset.eq(&other.offset)
    }
}

impl Eq for PatternBlock {}

impl PartialOrd for PatternBlock {
    #[inline] fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.offset.partial_cmp(&other.offset)
    }
}

impl Ord for PatternBlock {
    #[inline] fn cmp(&self, other: &Self) -> Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl PatternBlock {
    pub fn desc(&self) -> String {
        format!("{} @{:.3}, layer {}",
            self.sound.name(), *self.offset, self.layer)
    }
}

pub enum SequencerState {
    Start,
    Play{next: usize, start_time: Secs},
    Idle{start_time: Secs},
    Stop,
    None
}

pub struct Sequencer {
    pattern: Vec<PatternBlock>,
    pending: Vec<(usize, Secs)>,
    state: SequencerState,
    audio_ctx: AudioContext,
    visualiser: JsAnalyserNode,
    plug: DynamicsCompressorNode,
    gain: GainNode,
    bps: Beats
}

impl Sequencer {
    #[inline] pub fn new() -> JsResult<Self> {
        let audio_ctx = AudioContext::new().add_loc(loc!())?;
        let plug = DynamicsCompressorNode::new(&audio_ctx).add_loc(loc!())?;
        plug.ratio().set_value(20.0);
        plug.release().set_value(1.0);
        let gain = GainNode::new(&audio_ctx).add_loc(loc!())?;
        gain.gain().set_value(0.2);
        let visualiser = JsAnalyserNode::new(&audio_ctx).add_loc(loc!())?;
        visualiser.set_fft_size(512);

        plug.connect_with_audio_node(&visualiser).add_loc(loc!())?
            .connect_with_audio_node(&gain).add_loc(loc!())?
            .connect_with_audio_node(&audio_ctx.destination()).add_loc(loc!())?;

        Ok(Self{pattern: vec![], pending: vec![], audio_ctx,
            state: SequencerState::None, visualiser, plug, gain, bps: r64![2.0]})
    }

    #[inline] pub fn visualiser(&self) -> Option<&JsAnalyserNode> {
        matches!(self.state, SequencerState::None)
            .not().then_some(&self.visualiser)
    }

    #[inline] pub fn gain(&self) -> R32 {
        R32::new_or(R32::ZERO, self.gain.gain().value())
    }

    /// Beats Per Second
    #[inline] pub fn bps(&self) -> Beats {
        self.bps
    }

    #[inline] pub fn pattern(&self) -> &[PatternBlock] {
        &self.pattern
    }

    #[inline] pub fn pattern_mut(&mut self) -> &mut [PatternBlock] {
        &mut self.pattern
    }

    /// the returned `bool` indicates whether the selected element's editor window should be
    /// rerendered
    pub fn set_param(&mut self, id: ParamId, value: R64) -> JsResult<bool> {
        match id {
            ParamId::Add(ty, layer) => {
                let block = PatternBlock{
                    sound: ty.init(&self.audio_ctx).add_loc(loc!())?,
                    layer, offset: value};
                self.pattern.push_sorted(block);
            }

            ParamId::Remove(id) => if value.is_sign_negative() {
                self.pattern.try_remove(id)
                    .to_js_result(loc!())?;
            }

            ParamId::Play => if value.is_sign_positive() {
                self.state = SequencerState::Start;
                for block in self.pattern.iter_mut() {
                    block.sound.reset(&self.audio_ctx).add_loc(loc!())?;
                }
            } else {
                self.state = SequencerState::Stop;
            }

            ParamId::Bpm => self.bps = value / 60u8,

            ParamId::MasterGain => self.gain.gain().set_value(*value as f32),

            param_id => if let Some(block_id) = param_id.block_id() {
                return Ok(self.pattern.get_mut(block_id).to_js_result(loc!())?
                    .sound.set_param(param_id, value))
            }
        }
        Ok(false)
    }

    pub fn cur_play_offset(&self, time: Secs) -> Beats {
        if let SequencerState::Play{start_time, ..} | SequencerState::Idle{start_time} = self.state {
            (time - start_time).secs_to_beats(self.bps)
        } else {Beats::NEG_INFINITY}
    }

    pub fn poll(&mut self, time: Secs) -> JsResult<()> {
        match self.state {
            SequencerState::Start => {
                let mut next = 0;
                for block in self.pattern.iter_mut().take_while(|x| *x.offset == 0.0) {
                    let when = block.sound.poll(time, &self.plug, self.bps).add_loc(loc!())?;
                    self.pending.push_sorted_by_key((next, when), |x| x.0);
                    next += 1;
                }
                self.state = SequencerState::Play{next, start_time: time};
            }

            SequencerState::Play{ref mut next, start_time} => {
                let offset = (time - start_time).secs_to_beats(self.bps);
                for block in self.pattern.iter_mut().skip(*next).take_while(|x| x.offset <= offset) {
                    let when = block.sound.poll(time, &self.plug, self.bps).add_loc(loc!())?;
                    self.pending.push_sorted_by_key((*next, when), |x| x.0);
                    *next += 1;
                }
                if *next >= self.pattern.len() {
                    self.state = SequencerState::Idle{start_time};
                }
            }

            SequencerState::Stop => {
                for (id, _) in self.pending.drain(..) {
                    unsafe{self.pattern.get_unchecked_mut(id)}
                        .sound.stop(time).add_loc(loc!())?;
                }
                self.state = SequencerState::None;
            }

            SequencerState::Idle{..} | SequencerState::None => ()
        }

        let n_due = self.pending.iter().position(|x| x.1 > time).unwrap_or(self.pending.len());
        let due: Vec<usize> = self.pending.drain(..n_due).map(|x| x.0).collect::<Vec<_>>();
        for id in due {
            let when = unsafe{self.pattern.get_unchecked_mut(id)}
                .sound.poll(time, &self.plug, self.bps).add_loc(loc!())?;
            if when.is_infinite() {continue}
            self.pending.push_sorted_by_key((id, when), |x| x.0);
        }
        Ok(())
    }
}
