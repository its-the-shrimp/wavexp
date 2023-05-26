use std::{cmp::Reverse, fmt::Debug};
use web_sys::{AnalyserNode as JsAnalyserNode, GainOptions, DynamicsCompressorOptions, DynamicsCompressorNode, GainNode, AudioContext};
use crate::{
    sound::{SoundGen, Note, SoundPlayer, Secs, Sound, MSecs, Beats},
    utils::{JsResult, JsResultUtils, R64, OptionExt, VecExt, Take, ResultToJsResult},
    input::ParamId,
    loc, r32, r64
};

pub enum PatternBlock {
    Sound{sound: Sound, layer: usize},
    Delay(Beats)
}

pub struct Sequencer {
    pattern: Vec<PatternBlock>,
    pending: Vec<(usize, Secs)>,
    state: Option<usize>,
    visualiser: JsAnalyserNode,
    plug: DynamicsCompressorNode,
    gain: GainNode
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

        Ok(Self{pattern: vec![], state: None, pending: vec![],
            visualiser, plug, gain})
    }

    #[inline] pub fn visualiser(&self) -> &JsAnalyserNode {
        &self.visualiser
    }

    #[inline] pub fn pattern(&self) -> &[PatternBlock] {
        &self.pattern
    }

    pub fn add_block(&mut self, sound: Sound, layer: usize, offset: Beats) -> JsResult<()> {
        let mut acc = Beats::ZERO;
        for (i, x) in self.pattern.iter_mut().enumerate() {
            if let PatternBlock::Delay(x) = x {
                acc += *x;
                if offset <= acc {
                    if offset < acc {
                        *x = offset - (acc - *x);
                        self.pattern.try_insert(i + 1, PatternBlock::Delay(acc - offset))
                            .to_js_result(loc!())?;
                    }
                    self.pattern.try_insert(i + 1, PatternBlock::Sound{sound, layer})
                        .to_js_result(loc!())?;
                    return Ok(())
                }
            }
        }
        self.pattern.push(PatternBlock::Delay(offset - acc));
        self.pattern.push(PatternBlock::Sound{sound, layer});
        Ok(())
    }

    #[inline] pub fn start(&mut self) {
        self.state = Some(0);
    }

    /// the returned `bool` indicates whether the selected element's editor window should be
    /// rerendered
    pub fn set_param(&mut self, id: ParamId, value: R64) -> JsResult<bool> {
        match id {
            ParamId::Play(_) => 
                self.state = value.is_sign_positive().then_some(0),
            _ => ()
        }
        Ok(false)
    }

    pub fn poll(&mut self, time: Secs) -> JsResult<()> {
        if let Some(ref mut id) = self.state {
            let mut iter = self.pattern.iter_mut().skip(*id);
            while let Some(PatternBlock::Sound{sound, layer}) = iter.next() {
                let when = sound.poll(time).add_loc(loc!())?;
                self.pending.push_sorted_by_key((*id, when), |x| x.0);
                *id += 1;
            }
            if iter.next().is_none() {
                self.state = None;
            }
        } else {
            for (id, _) in self.pending.drain(..) {
                let Some(PatternBlock::Sound{sound, ..}) = self.pattern.get_mut(id)
                    else {continue};
                sound.stop(time).add_loc(loc!())?;
            }
        }
        Ok(())
    }
}
