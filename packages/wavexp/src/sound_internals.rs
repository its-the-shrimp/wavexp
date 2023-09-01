use std::{
    ops::Deref,
    rc::Rc,
    fmt::{Display, Formatter, self}};
use wasm_bindgen::{link_to, JsCast};
use wasm_bindgen_futures::JsFuture;
use wavexp_utils::{AppResult, R64, SharedExt};
use web_sys::{
    AudioWorklet,
    BaseAudioContext,
    AudioWorkletNode,
    AudioParam,
    AudioBuffer,
    File};
use yew::scheduler::Shared;
use crate::{
    sound::{Secs, Beats, FromBeats},
    sequencer::Sequencer};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum AudioInputKind {
    #[default] Empty,
    /// the inner string is the name of the file
    File(Rc<str>)
}

impl Display for AudioInputKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "None"),
            Self::File(name) => write!(f, "File {name:?}")
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct AudioInput {
    kind: AudioInputKind,
    inner: Option<AudioBuffer>,
    duration: Secs,
    playing: bool
}

impl AudioInput {
    pub async fn new_file(file: File, sequencer: Shared<Sequencer>) -> AppResult<Self> {
        let raw = JsFuture::from(file.array_buffer()).await?.dyn_into()?;
        let ctx = sequencer.get().map(|s| s.audio_ctx().clone())?;
        let inner: AudioBuffer = JsFuture::from(ctx.decode_audio_data(&raw)?).await?.dyn_into()?;
        let kind = AudioInputKind::File(file.name().into());
        let duration = R64::try_from(inner.duration())?;
        Ok(Self{kind, inner: Some(inner), duration, playing: false})
    }

    pub fn kind(&self) -> &AudioInputKind {&self.kind}
    pub fn duration(&self) -> Secs {self.duration}
    pub fn inner(&self) -> Option<&AudioBuffer> {self.inner.as_ref()}
    pub fn playing(&self) -> bool {self.playing}
    pub fn set_playing(&mut self, new: bool) {self.playing = new}

    pub fn add_ctx<'this, 'ctx>(&'this self, ctx: &'ctx Sequencer) -> AudioInputWithCtx<'this, 'ctx> {
        AudioInputWithCtx{this: self, ctx}
    }
}

#[derive(Clone, Copy)]
pub struct AudioInputWithCtx<'this, 'ctx> {
    pub this: &'this AudioInput,
    pub ctx: &'ctx Sequencer
}

impl<'this, 'ctx> AudioInputWithCtx<'this, 'ctx> {
    pub fn duration(&self) -> Beats {
        self.this.duration.secs_to_beats(self.ctx.bps())
    }
}

impl<'this, 'ctx> Display for AudioInputWithCtx<'this, 'ctx> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.this.kind.fmt(f)?;
        write!(f, ", {:.2} beats", self.this.duration)
    }
}

pub struct TimeStretcherNode(AudioWorkletNode);

impl Deref for TimeStretcherNode {
    type Target = AudioWorkletNode;
    fn deref(&self) -> &Self::Target {&self.0}
}

impl TimeStretcherNode {
    pub async fn register(worklet: AudioWorklet) -> AppResult<()> {
        JsFuture::from(worklet.add_module(&link_to!(inline_js = r#"
            class TimeStretcher extends AudioWorkletProcessor {
                constructor() {
                    super();
                    this.cache = [];
                }

                static get parameterDescriptors() {
                    return [
                        {
                            name: "rate",
                            defaultValue: 1,
                            minValue: 0,
                            maxValue: 50,
                            automationRate: "k-rate",
                        },
                    ];
                }

                process(inputs, outputs, parameters) {
                    let n_inputs = inputs.length;
                    let rate = parameters["rate"][0];
                    for (let i = 0; i < n_inputs; i++) {
                        let input = inputs[i];
                        let n_channels = input.length;
                        for (let c = 0; c < n_channels; c++) {
                            let channel = input[c];
                            let n_samples = channel.length;
                            let last = channel[n_samples - 1];
                            for (let s = 0; s < n_samples; s++) {
                                let s_applied = s / rate;
                                let lower = Math.floor(s_applied);
                                let upper = Math.ceil(s_applied);
                                let upper_weight = s_applied % 1;
                                let lower_weight = 1 - upper_weight;
                                outputs[i][c][s] =
                                    (channel[lower] || last) * lower_weight
                                  + (channel[upper] || last) * upper_weight;
                            }
                        }
                    }
                    return true;
                }
            }

            registerProcessor("time-stretcher", TimeStretcher);
        "#))?).await?;
        Ok(())
    }

    pub fn new(ctx: &BaseAudioContext) -> AppResult<Self> {
        Ok(Self(AudioWorkletNode::new(ctx, "time-stretcher")?))
    }

    pub fn rate(&self) -> AudioParam {
        unsafe {
            self.0.parameters().unwrap_unchecked()
                .get("rate").unwrap_unchecked()
        }
    }
}
