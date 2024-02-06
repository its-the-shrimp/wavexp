use super::CustomBlock;
use crate::{
    ctx::{AppEvent, ContextMut, ContextRef, EditorAction},
    input::{Counter, GraphEditorCanvas, Slider},
    sequencer::{PlaybackContext, Sequencer},
    sound::{Beats, FromBeats, Note, Secs},
    visual::{GraphEditor, GraphPoint},
};
use js_sys::Math::random;
use macro_rules_attribute::apply;
use std::{array::from_fn, cell::LazyCell, mem::replace, num::NonZeroU32, ops::RangeBounds};
use wasm_bindgen::JsCast;
use wavexp_utils::{
    cell::{Shared, WasmCell},
    error::{AppError, Result},
    ext::default,
    ext::{ArrayExt, OptionExt, ResultExt},
    fallible, js_function, r32, r64,
    range::{RangeBoundsExt, RangeInclusiveV2, RangeV2},
    real::R32,
    real::R64,
    ArrayFrom,
};
use web_sys::{AudioBuffer, AudioBufferOptions, AudioNode, Path2d};
use yew::{html, Html};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NoiseBlock {
    pub offset: Beats,
    pub pitch: Note,
    pub len: Beats,
}

impl GraphPoint for NoiseBlock {
    const EDITOR_NAME: &'static str = CustomBlock::EDITOR_NAME;
    const Y_BOUND: RangeV2<R64> = CustomBlock::Y_BOUND;
    const SCALE_Y_BOUND: RangeV2<R64> = CustomBlock::SCALE_Y_BOUND;
    const OFFSET_Y_BOUND: RangeV2<R64> = CustomBlock::OFFSET_Y_BOUND;
    const Y_SNAP: R64 = CustomBlock::Y_SNAP;

    type Inner = Beats;
    type Y = Note;
    /// (sound block offset, number of repetitions of the pattern)
    type VisualContext = (Beats, NonZeroU32);

    fn create(_: &GraphEditor<Self>, [offset, y]: [R64; 2]) -> Self {
        Self {
            offset,
            pitch: Note::saturated(y.into()).recip(),
            len: r64!(1),
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

    #[apply(fallible!)]
    fn móve(&mut self, delta: [R64; 2], meta: bool) {
        if meta {
            self.len += delta[0];
        } else {
            self.offset += delta[0];
            self.offset = R64::ZERO.max(self.offset);
        }
        self.pitch = (self.pitch - isize::from(delta[1]))?;
    }

    fn move_point(point: &mut [R64; 2], delta: [R64; 2], meta: bool) {
        if !meta {
            point[0] += delta[0];
        }
        point[1] += delta[1]
    }

    #[apply(fallible!)]
    fn in_hitbox(
        &self,
        area: &[RangeInclusiveV2<R64>; 2],
        _: ContextRef,
        _: &Sequencer,
        _: Self::VisualContext,
    ) -> bool {
        area[1]
            .map_bounds(usize::from)
            .contains(&self.pitch.recip().index())
            && (self.offset..=self.offset + self.len).overlap(&area[0])
    }

    #[apply(fallible!)]
    fn on_redraw(
        editor: &mut GraphEditor<Self>,
        ctx: ContextRef,
        sequencer: &Sequencer,
        canvas_size: &[R64; 2],
        solid: &Path2d,
        _: &Path2d,
        (sb_offset, n_reps): Self::VisualContext,
    ) {
        let bps = sequencer.bps();
        let step = canvas_size.div(editor.scale());
        let offset = R64::array_from(editor.offset());
        for block in editor.data() {
            let [x, y] = block.loc().mul(step).sub(offset);
            solid.rect(*x, *y, *block.len * *step[0], *step[1]);
        }

        let total_len = editor.data().last().map_or_default(|x| x.offset + x.len);
        if let PlaybackContext::All(start) = sequencer.playback_ctx() && start.is_finite() {
            let progress = (ctx.frame() - start).secs_to_beats(bps) - sb_offset;
            if progress < total_len * n_reps {
                editor.force_redraw();
                let x = R64::new_or(progress, *progress % *total_len) * step[0] - offset[0];
                solid.move_to(*x, 0.0);
                solid.line_to(*x, *canvas_size[1]);
            }
        }
    }

    fn fmt_loc(loc: [R64; 2]) -> String {
        CustomBlock::fmt_loc(loc)
    }
}

#[derive(Debug, Clone)]
pub struct NoiseSound {
    pub pattern: Shared<GraphEditor<NoiseBlock>>,
    pub volume: R32,
    pub attack: Beats,
    pub decay: Beats,
    pub sustain: R32,
    pub release: Beats,
    pub rep_count: NonZeroU32,
}

impl Default for NoiseSound {
    fn default() -> Self {
        Self {
            pattern: default(),
            volume: r32!(0.2),
            attack: r64!(0),
            decay: r64!(0),
            sustain: r32!(1),
            release: r64!(0.2),
            rep_count: NonZeroU32::MIN,
        }
    }
}

static NOISE: WasmCell<LazyCell<Option<AudioBuffer>>> = WasmCell(LazyCell::new(|| {
    let res: Result<AudioBuffer> = try {
        let buf: [f32; Sequencer::SAMPLE_RATE as usize] = from_fn(|_| random() as f32 * 2.0 - 1.0);
        let res = AudioBuffer::new(
            AudioBufferOptions::new(Sequencer::SAMPLE_RATE, Sequencer::SAMPLE_RATE as f32)
                .number_of_channels(Sequencer::CHANNEL_COUNT),
        )?;
        for i in 0..Sequencer::CHANNEL_COUNT as i32 {
            res.copy_to_channel(&buf, i)?;
        }
        res
    };
    res.report()
}));

impl NoiseSound {
    pub const NAME: &'static str = "White Noise";

    pub fn play(&self, plug: &AudioNode, now: Secs, self_offset: Secs, bps: Beats) -> Result {
        let pat = self.pattern.get()?;
        let Some(last) = pat.data().last() else {
            return Ok(());
        };
        let pat_len = (last.offset + last.len).to_secs(bps);
        let ctx = plug.context();

        for rep in 0..self.rep_count.get() {
            for NoiseBlock { offset, len, pitch } in pat.data() {
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
                block_core.set_buffer(NOISE.as_ref());
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
        }
        Ok(())
    }

    pub fn len(&self) -> Result<Beats> {
        Ok(self
            .pattern
            .get()?
            .data()
            .last()
            .map_or_default(|x| x.offset + x.len))
    }

    pub const fn rep_count(&self) -> NonZeroU32 {
        self.rep_count
    }

    pub fn params(&self, ctx: ContextRef) -> Html {
        let emitter = ctx.event_emitter();
        match ctx.selected_tab() {
            0 /* General */ => html!{
                <div
                    id="inputs"
                >
                    <Slider
                        key="noise-vol"
                        setter={emitter.reform(|x| AppEvent::Volume(R32::from(x)))}
                        name="Noise Volume"
                        initial={self.volume}
                    />
                    <Counter
                        key="noise-repcnt"
                        setter={emitter.reform(|x| AppEvent::RepCount(NonZeroU32::from(x)))}
                        fmt={|x| format!("{x:.0}")}
                        name="Number Of Pattern Repetitions"
                        min=1
                        initial={self.rep_count}
                    />
                </div>
            },

            1 /* Envelope */ => html!{
                <div
                    id="inputs"
                >
                    <Counter
                        key="noise-att"
                        setter={emitter.reform(AppEvent::Attack)}
                        name="Noise Attack Time"
                        postfix="Beats"
                        initial={self.attack}
                    />
                    <Counter
                        key="noise-dec"
                        setter={emitter.reform(AppEvent::Decay)}
                        name="Noise Decay Time"
                        postfix="Beats"
                        initial={self.decay}
                    />
                    <Slider
                        key="noise-sus"
                        setter={emitter.reform(|x| AppEvent::Sustain(R32::from(x)))}
                        name="Noise Sustain Level"
                        initial={self.sustain}
                    />
                    <Counter
                        key="noise-rel"
                        setter={emitter.reform(AppEvent::Release)}
                        name="Noise Release Time"
                        postfix="Beats"
                        initial={self.release}
                    />
                </div>
            },

            2 /* Pattern */ => html!{
                <GraphEditorCanvas<NoiseBlock> editor={&self.pattern} {emitter} />
            },

            tab_id => html!{ <p style="color:red">{ format!("Invalid tab ID: {tab_id}") }</p> }
        }
    }

    /// `reset_sound` is set to `false` initially,
    /// if set to true, resets the sound block to an `Undefined` type
    #[apply(fallible!)]
    pub fn handle_event(
        &mut self,
        event: &AppEvent,
        mut ctx: ContextMut,
        sequencer: &Sequencer,
        reset_sound: &mut bool,
        offset: Beats,
    ) {
        match *event {
            AppEvent::Volume(to) => ctx.register_action(EditorAction::SetVolume {
                from: replace(&mut self.volume, to),
                to,
            })?,

            AppEvent::Attack(to) => ctx.register_action(EditorAction::SetAttack {
                from: replace(&mut self.attack, to),
                to,
            })?,

            AppEvent::Decay(to) => ctx.register_action(EditorAction::SetDecay {
                from: replace(&mut self.decay, to),
                to,
            })?,

            AppEvent::Sustain(to) => ctx.register_action(EditorAction::SetSustain {
                from: replace(&mut self.sustain, to),
                to,
            })?,

            AppEvent::Release(to) => ctx.register_action(EditorAction::SetRelease {
                from: replace(&mut self.release, to),
                to,
            })?,

            AppEvent::RepCount(to) => {
                ctx.register_action(EditorAction::SetRepCount {
                    from: replace(&mut self.rep_count, to),
                    to,
                })?;
                ctx.emit_event(AppEvent::RedrawEditorPlane);
            }

            AppEvent::Undo(ref actions) => {
                let mut pat = self.pattern.get_mut()?;
                for action in actions.iter() {
                    match *action {
                        EditorAction::SetBlockType(_) => {
                            *reset_sound = true;
                            break;
                        }

                        EditorAction::SetVolume { from, .. } => self.volume = from,

                        EditorAction::SetAttack { from, .. } => self.attack = from,

                        EditorAction::SetDecay { from, .. } => self.decay = from,

                        EditorAction::SetSustain { from, .. } => self.sustain = from,

                        EditorAction::SetRelease { from, .. } => self.release = from,

                        EditorAction::SetRepCount { from, .. } => {
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
                        EditorAction::SetVolume { to, .. } => self.volume = to,

                        EditorAction::SetAttack { to, .. } => self.attack = to,

                        EditorAction::SetDecay { to, .. } => self.decay = to,

                        EditorAction::SetSustain { to, .. } => self.sustain = to,

                        EditorAction::SetRelease { to, .. } => self.release = to,

                        EditorAction::SetRepCount { to, .. } => {
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
        }
    }
}
