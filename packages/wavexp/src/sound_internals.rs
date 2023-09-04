use std::{
    ops::Deref,
    rc::Rc,
    mem::replace};
use wasm_bindgen::{link_to, JsCast};
use wasm_bindgen_futures::JsFuture;
use wavexp_utils::{AppResult, R64, SharedExt, default};
use web_sys::{
    AudioWorklet,
    BaseAudioContext,
    AudioWorkletNode,
    AudioParam,
    AudioBuffer,
    File,
    AudioBufferOptions};
use yew::scheduler::Shared;
use crate::{
    sound::{Secs, Beats, FromBeats},
    sequencer::Sequencer};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct AudioInputChanges {
    /// Make the input play backwards.
    pub reversed: bool
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AudioInput {
    name: Rc<str>,
    duration: Secs,
    raw: AudioBuffer,
    pending_changes: AudioInputChanges,
    baked_changes: AudioInputChanges,
    baked: AudioBuffer
}

impl AudioInput {
    pub async fn new_file(file: File, sequencer: Shared<Sequencer>) -> AppResult<Self> {
        let raw = JsFuture::from(file.array_buffer()).await?.dyn_into()?;
        let ctx = sequencer.get().map(|s| s.audio_ctx().clone())?;
        let mut raw: AudioBuffer = JsFuture::from(ctx.decode_audio_data(&raw)?).await?.dyn_into()?;
        if raw.number_of_channels() < Sequencer::CHANNEL_COUNT {
            let new_raw = AudioBuffer::new(
                AudioBufferOptions::new(raw.length(), Sequencer::SAMPLE_RATE as f32)
                    .number_of_channels(Sequencer::CHANNEL_COUNT))?;
            let data = raw.get_channel_data(0)?;
            for i in 0 .. Sequencer::CHANNEL_COUNT as i32 {
                new_raw.copy_to_channel(&data, i)?;
            }
            raw = new_raw;
        }
        let name = format!("File {:?}", file.name()).into();
        let duration = R64::try_from(raw.duration())?;
        Ok(Self{name, baked: raw.clone(), raw, duration, pending_changes: default(), baked_changes: default()})
    }

    /// Name of the input, exists solely for the user's convenience.
    pub fn name(&self) -> &Rc<str> {&self.name}
    /// Sets the name of the input, returning the old one.
    pub fn set_name(&mut self, name: Rc<str>) -> Rc<str> {replace(&mut self.name, name)}
    // /// Duration of the raw buffer, unchanged since the moment the input was created.
    // pub fn raw_duration(&self) -> Secs {self.duration}
    /// Duration of the buffer with all the requested changes baked in.
    pub fn baked_duration(&self) -> Secs {self.duration}

    // /// Raw buffer, unchanged since the moment the input was created.
    // pub fn raw(&self) -> &AudioBuffer {&self.raw}

    /// Get a struct holding all the changes yet to be baked into the input.
    pub fn changes(&self) -> AudioInputChanges {self.pending_changes}
    /// Get a mutable reference to a struct holding all the changes yet to be baked into the input.
    pub fn changes_mut(&mut self) -> &mut AudioInputChanges {&mut self.pending_changes}

    /// Bake all of the changes into a buffer that will be accessible through `.baked()` method.
    /// If an error occurs, the input will appear unbaked.
    pub fn bake(&mut self) -> AppResult<()> {
        if self.pending_changes == self.baked_changes {return Ok(())};
        self.baked = AudioBuffer::new(
            AudioBufferOptions::new(self.raw.length(), Sequencer::SAMPLE_RATE as f32)
                .number_of_channels(Sequencer::CHANNEL_COUNT))?;
        for i in 0 .. Sequencer::CHANNEL_COUNT {
            let mut data = self.raw.get_channel_data(i)?;
            if self.pending_changes.reversed {
                data.reverse();
            }
            self.baked.copy_to_channel(&data, i as i32)?;
        }
        Ok(self.baked_changes = self.pending_changes)
    }

    /// Buffer with all the requested changes baked in.
    /// If the there are unbaked changes, `None` is returned.
    pub fn baked(&self) -> Option<&AudioBuffer> {
        (self.pending_changes == self.baked_changes).then_some(&self.baked)
    }

    pub fn desc(&self, bps: Beats) -> String {
        format!("{}, {:.2} beats", self.name, self.duration.secs_to_beats(bps))
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
