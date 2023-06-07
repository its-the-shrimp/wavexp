use std::cmp::Ordering;
use web_sys::{AnalyserNode as JsAnalyserNode, GainOptions, DynamicsCompressorOptions, DynamicsCompressorNode, GainNode, AudioContext};
use crate::{
    sound::{Secs, Sound, Beats, MSecs, FromBeats},
    utils::{JsResult, JsResultUtils, R64, VecExt, Check, R32, OptionExt, ResultToJsResult},
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

pub struct Sequencer {
    pattern: Vec<PatternBlock>,
    pending: Vec<(usize, Secs)>,
    state: Option<usize>,
    start_time: Secs,
    audio_ctx: AudioContext,
    visualiser: JsAnalyserNode,
    plug: DynamicsCompressorNode,
    gain: GainNode,
    bps: Beats
}

impl Sequencer {
    #[inline] pub fn new() -> JsResult<Self> {
        let audio_ctx = AudioContext::new().add_loc(loc!())?;
        let plug = DynamicsCompressorNode::new_with_options(&audio_ctx,
            DynamicsCompressorOptions::new().ratio(20.0).release(1.0)).add_loc(loc!())?;
        let gain = GainNode::new_with_options(&audio_ctx,
            GainOptions::new().gain(0.2)).add_loc(loc!())?;
        let visualiser = JsAnalyserNode::new(&audio_ctx).add_loc(loc!())?;

        plug.connect_with_audio_node(&visualiser).add_loc(loc!())?
            .connect_with_audio_node(&gain).add_loc(loc!())?
            .connect_with_audio_node(&audio_ctx.destination()).add_loc(loc!())?;

        Ok(Self{pattern: vec![], state: None, pending: vec![], audio_ctx,
            start_time: R64::INFINITY, visualiser, plug, gain, bps: r64![2.0]})
    }

    #[inline] pub fn visualiser(&self) -> Option<&JsAnalyserNode> {
        self.state.is_some().then_some(&self.visualiser)
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
                self.state = Some(0);
                self.start_time = R64::INFINITY;
                for block in self.pattern.iter_mut() {
                    block.sound.reset().add_loc(loc!())?;
                }
            } else {
                self.state = None;
                self.start_time = R64::NEG_INFINITY;
            }

            ParamId::Bpm =>
                self.bps = value / 60u8,

            ParamId::MasterGain =>
                self.gain.gain().set_value(*value as f32),

            param_id => if let Some(block_id) = param_id.block_id() {
                return Ok(self.pattern.get_mut(block_id).to_js_result(loc!())?
                    .sound.set_param(param_id, value))
            }
        }
        Ok(false)
    }

    pub fn poll(&mut self, time: MSecs) -> JsResult<()> {
        let time: Secs = time / 1000;
        if let Some(ref mut id) = self.state {
            self.start_time = self.start_time.check(Secs::is_finite)
                .unwrap_or(time);
            let offset = time - self.start_time;
            for block in self.pattern.iter_mut().skip(*id).take_while(|x| x.offset.to_secs(self.bps) <= offset) {
                let when = block.sound.poll(time, &self.plug, self.bps).add_loc(loc!())?;
                self.pending.push_sorted_by_key((*id, when), |x| x.0);
                *id += 1;
            }
            if *id >= self.pattern.len() {
                // TODO: make it optionally loop around
                self.state = None;
            }
        } else if self.start_time.is_sign_negative() {
            for (id, _) in self.pending.drain(..) {
                unsafe{self.pattern.get_unchecked_mut(id)}
                    .sound.stop(time).add_loc(loc!())?;
            }
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
