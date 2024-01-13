use super::CustomBlock;
use crate::{
    global::{AppAction, AppContext, AppEvent},
    input::{Counter, GraphEditorCanvas, Slider},
    sequencer::{PlaybackContext, Sequencer},
    sound::{Beats, FromBeats, Note, Secs},
    visual::{GraphEditor, GraphPoint},
};
use js_sys::Math::random;
use std::{
    mem::replace,
    num::NonZeroUsize,
    ops::{Range, RangeInclusive},
};
use wasm_bindgen::JsCast;
use wavexp_utils::{
    cell::{Shared, SharedAwareRefMut},
    default, js_function, r32, r64, AppError, AppResult, AppResultUtils, ArrayExt, ArrayFrom,
    OptionExt, Pipe, RangeExt, R32, R64,
};
use web_sys::{AudioBuffer, AudioBufferOptions, AudioNode, Path2d};
use yew::{html, Html};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NoiseBlock {
    offset: Beats,
    pitch: Note,
    len: Beats,
}

impl GraphPoint for NoiseBlock {
    const EDITOR_NAME: &'static str = CustomBlock::EDITOR_NAME;
    const Y_BOUND: Range<R64> = CustomBlock::Y_BOUND;
    const SCALE_Y_BOUND: Range<R64> = CustomBlock::SCALE_Y_BOUND;
    const OFFSET_Y_BOUND: Range<R64> = CustomBlock::OFFSET_Y_BOUND;
    const Y_SNAP: R64 = CustomBlock::Y_SNAP;

    type Inner = Beats;
    type Y = Note;
    /// (sound block offset, number of repetitions of the pattern)
    type VisualContext = (Beats, NonZeroUsize);

    fn create(_: &GraphEditor<Self>, [offset, y]: [R64; 2]) -> Self {
        Self {
            offset,
            pitch: Note::from_index(y.into()).recip(),
            len: r64![1],
        }
    }

    fn inner(&self) -> &Self::Inner {
        &self.len
    }
    fn inner_mut(&mut self) -> &mut Self::Inner {
        &mut self.len
    }
    fn y(&self) -> &Self::Y {
        &self.pitch
    }
    fn y_mut(&mut self) -> &mut Self::Y {
        &mut self.pitch
    }
    fn loc(&self) -> [R64; 2] {
        [self.offset, self.pitch.recip().index().into()]
    }

    fn move_point(point: Result<&mut Self, &mut [R64; 2]>, delta: [R64; 2], meta: bool) {
        match point {
            Ok(NoiseBlock { offset, pitch, len }) => {
                if meta {
                    *len += delta[0];
                } else {
                    *offset += delta[0];
                    *offset = R64::ZERO.max(*offset);
                }
                *pitch -= delta[1].into();
            }
            Err(point) => {
                if !meta {
                    point[0] += delta[0];
                }
                point[1] += delta[1];
            }
        }
    }

    fn in_hitbox(
        &self,
        area: &[RangeInclusive<R64>; 2],
        _: &AppContext,
        _: &Sequencer,
        _: Self::VisualContext,
    ) -> AppResult<bool> {
        Ok(area[1]
            .clone()
            .map_bounds(usize::from)
            .contains(&self.pitch.recip().index())
            && (self.offset..=self.offset + self.len).overlap(&area[0]))
    }

    fn on_redraw(
        editor: &mut GraphEditor<Self>,
        ctx: &AppContext,
        sequencer: &Sequencer,
        canvas_size: &[R64; 2],
        solid: &Path2d,
        _: &Path2d,
        (sb_offset, n_reps): Self::VisualContext,
    ) -> AppResult<()> {
        let bps = sequencer.bps();
        let step = &canvas_size.div(&editor.scale());
        let offset = &R64::array_from(editor.offset());
        for block in editor.iter() {
            let [x, y] = block.loc().mul(step).sub(offset);
            solid.rect(*x, *y, *block.len * *step[0], *step[1]);
        }

        let total_len = editor.last().map_or_default(|x| x.offset + x.len);
        Ok(if let PlaybackContext::All(start) = sequencer.playback_ctx() && start.is_finite() {
            let progress = (ctx.frame() - start).secs_to_beats(bps) - sb_offset;
            if progress < total_len * n_reps {
                editor.force_redraw();
                let x = R64::new_or(progress, *progress % *total_len) * step[0] - offset[0];
                solid.move_to(*x, 0.0);
                solid.line_to(*x, *canvas_size[1]);
            }
        })
    }

    fn fmt_loc(loc: [R64; 2]) -> String {
        CustomBlock::fmt_loc(loc)
    }
}

#[derive(Debug, Clone)]
pub struct NoiseSound {
    pattern: Shared<GraphEditor<NoiseBlock>>,
    src: AudioBuffer,
    volume: R32,
    attack: Beats,
    decay: Beats,
    sustain: R32,
    release: Beats,
    rep_count: NonZeroUsize,
}

impl NoiseSound {
    pub const NAME: &'static str = "White Noise";

    pub fn prepare(&self) -> AppResult<()> {
        Ok(())
    }

    pub fn new() -> AppResult<Self> {
        let mut buf = vec![0.0; Sequencer::SAMPLE_RATE as usize]; // 1 second of noise
        buf.fill_with(|| random() as f32 * 2.0 - 1.0);
        let src = AudioBufferOptions::new(Sequencer::SAMPLE_RATE, Sequencer::SAMPLE_RATE as f32)
            .number_of_channels(Sequencer::CHANNEL_COUNT)
            .pipe(|x| AudioBuffer::new(x))?;
        for i in 0..Sequencer::CHANNEL_COUNT as i32 {
            src.copy_to_channel(&buf, i)?;
        }
        Ok(Self {
            pattern: default(),
            src,
            volume: r32![0.2],
            attack: r64![0],
            decay: r64![0],
            sustain: r32![1],
            release: r64![0.2],
            rep_count: NonZeroUsize::MIN,
        })
    }

    pub fn play(
        &self,
        plug: &AudioNode,
        now: Secs,
        self_offset: Secs,
        bps: Beats,
    ) -> AppResult<()> {
        let pat = self.pattern.get()?;
        let Some(last) = pat.last() else {
            return Ok(());
        };
        let pat_len = (last.offset + last.len).to_secs(bps);
        let ctx = plug.context();

        Ok(for rep in 0..self.rep_count.get() {
            for NoiseBlock { offset, len, pitch } in pat.iter() {
                let block = ctx.create_gain()?;
                let gain = block.gain();
                let start = now + self_offset + pat_len * rep + offset.to_secs(bps);
                let mut at = start;
                gain.set_value(0.0);
                at += self.attack.to_secs(bps);
                gain.linear_ramp_to_value_at_time(*self.volume, *at)?;
                at += self.decay.to_secs(bps);
                let sus = self.sustain * self.volume;
                gain.linear_ramp_to_value_at_time(*sus, *at)?;
                at = start + len.to_secs(bps);
                gain.set_value_at_time(*sus, *at - *self.release.to_secs(bps))?;
                gain.linear_ramp_to_value_at_time(0.0, *at)?;

                let block_core = ctx.create_buffer_source()?;
                block_core.set_buffer(Some(&self.src));
                block_core
                    .playback_rate()
                    .set_value(*pitch.pitch_coef() as f32);
                block_core.set_loop(true);
                block_core
                    .connect_with_audio_node(&block)?
                    .connect_with_audio_node(plug)?;
                block_core.start_with_when(*start)?;
                block_core.stop_with_when(*at)?;
                block_core.clone().set_onended(Some(&js_function!(|| {
                    block.disconnect().map_err(AppError::from).report();
                    block_core.disconnect().map_err(AppError::from).report();
                })));
            }
        })
    }

    pub fn len(&self, _: Beats) -> AppResult<Beats> {
        Ok(self
            .pattern
            .get()?
            .last()
            .map_or_default(|x| x.offset + x.len))
    }

    pub fn rep_count(&self) -> NonZeroUsize {
        self.rep_count
    }

    pub fn params(&self, ctx: &AppContext, _: &Sequencer) -> Html {
        let emitter = ctx.event_emitter();
        match ctx.selected_tab() {
            0 /* General */ => html!{<div id="inputs">
                <Slider key="noise-vol"
                setter={emitter.reform(|x| AppEvent::Volume(R32::from(x)))}
                name="Noise Volume"
                initial={self.volume}/>
                <Counter key="noise-repcnt"
                setter={emitter.reform(|x| AppEvent::RepCount(NonZeroUsize::from(x)))}
                fmt={|x| format!("{x:.0}")}
                name="Number Of Pattern Repetitions"
                min={r64![1]}
                initial={self.rep_count}/>
            </div>},

            1 /* Envelope */ => html!{<div id="inputs">
                <Counter key="noise-att"
                setter={emitter.reform(AppEvent::Attack)}
                name="Noise Attack Time" postfix="Beats"
                initial={self.attack}/>
                <Counter key="noise-dec"
                setter={emitter.reform(AppEvent::Decay)}
                name="Noise Decay Time" postfix="Beats"
                initial={self.decay}/>
                <Slider key="noise-sus"
                setter={emitter.reform(|x| AppEvent::Sustain(R32::from(x)))}
                name="Noise Sustain Level"
                initial={self.sustain}/>
                <Counter key="noise-rel"
                setter={emitter.reform(AppEvent::Release)}
                name="Noise Release Time" postfix="Beats"
                initial={self.release}/>
            </div>},

            2 /* Pattern */ => html!{
                <GraphEditorCanvas<NoiseBlock> editor={&self.pattern} {emitter}/>
            },

            tab_id => html!{<p style="color:red">{format!("Invalid tab ID: {tab_id}")}</p>}
        }
    }

    /// `reset_sound` is set to `false` initially,
    /// if set to true, resets the sound block to an `Undefined` type
    pub fn handle_event(
        &mut self,
        event: &AppEvent,
        ctx: &mut AppContext,
        sequencer: &SharedAwareRefMut<'_, Sequencer>,
        reset_sound: &mut bool,
        offset: Beats,
    ) -> AppResult<()> {
        Ok(match *event {
            AppEvent::Volume(to) => ctx.register_action(AppAction::SetVolume {
                from: replace(&mut self.volume, to),
                to,
            }),

            AppEvent::Attack(to) => ctx.register_action(AppAction::SetAttack {
                from: replace(&mut self.attack, to),
                to,
            }),

            AppEvent::Decay(to) => ctx.register_action(AppAction::SetDecay {
                from: replace(&mut self.decay, to),
                to,
            }),

            AppEvent::Sustain(to) => ctx.register_action(AppAction::SetSustain {
                from: replace(&mut self.sustain, to),
                to,
            }),

            AppEvent::Release(to) => ctx.register_action(AppAction::SetRelease {
                from: replace(&mut self.release, to),
                to,
            }),

            AppEvent::RepCount(to) => {
                ctx.register_action(AppAction::SetRepCount {
                    from: replace(&mut self.rep_count, to),
                    to,
                });
                ctx.emit_event(AppEvent::RedrawEditorPlane);
            }

            AppEvent::Undo(ref actions) => {
                let mut pat = self.pattern.get_mut()?;
                for action in actions.iter() {
                    match *action {
                        AppAction::SetBlockType(_) => {
                            *reset_sound = true;
                            break;
                        }

                        AppAction::SetVolume { from, .. } => self.volume = from,

                        AppAction::SetAttack { from, .. } => self.attack = from,

                        AppAction::SetDecay { from, .. } => self.decay = from,

                        AppAction::SetSustain { from, .. } => self.sustain = from,

                        AppAction::SetRelease { from, .. } => self.release = from,

                        AppAction::SetRepCount { from, .. } => {
                            self.rep_count = from;
                            ctx.emit_event(AppEvent::RedrawEditorPlane)
                        }

                        _ => (),
                    }
                }

                if ctx.selected_tab() == 2 {
                    pat.handle_event(event, ctx, sequencer, || (offset, self.rep_count))?;
                }
            }

            AppEvent::Redo(ref actions) => {
                let mut pat = self.pattern.get_mut()?;
                for action in actions.iter() {
                    match *action {
                        AppAction::SetVolume { to, .. } => self.volume = to,

                        AppAction::SetAttack { to, .. } => self.attack = to,

                        AppAction::SetDecay { to, .. } => self.decay = to,

                        AppAction::SetSustain { to, .. } => self.sustain = to,

                        AppAction::SetRelease { to, .. } => self.release = to,

                        AppAction::SetRepCount { to, .. } => {
                            self.rep_count = to;
                            ctx.emit_event(AppEvent::RedrawEditorPlane)
                        }

                        _ => (),
                    }
                }

                if ctx.selected_tab() == 2 {
                    pat.handle_event(event, ctx, sequencer, || (offset, self.rep_count))?;
                }
            }

            _ => {
                if ctx.selected_tab() == 2 {
                    self.pattern
                        .get_mut()?
                        .handle_event(event, ctx, sequencer, || (offset, self.rep_count))?;
                }
            }
        })
    }
}
