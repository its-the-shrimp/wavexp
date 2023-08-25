use std::{
    ops::Deref,
    rc::Rc,
    fmt::{Display, Formatter, self}};
use wasm_bindgen::link_to;
use wasm_bindgen_futures::JsFuture;
use wavexp_utils::AppResult;
use web_sys::{
    AudioWorklet,
    BaseAudioContext,
    AudioWorkletNode,
    AudioParam,
    AudioBuffer};
use crate::sound::{Secs, Beats};

#[derive(Debug, Clone)]
pub enum AudioInputKind {
    /// the inner string is the name of the file
    File(Rc<str>)
}

impl Display for AudioInputKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::File(name) => write!(f, "File {name:?}")
        }
    }
}

#[derive(Clone)]
pub struct AudioInput {
    kind: AudioInputKind,
    inner: AudioBuffer,
    duration: Beats
}

impl AudioInput {
    pub fn kind(&self) -> &AudioInputKind {&self.kind}
    pub fn duraion(&self) -> Secs {self.duration}
}

impl Display for AudioInput {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)?;
        write!(f, ", {:.2} beats", self.duration)
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
