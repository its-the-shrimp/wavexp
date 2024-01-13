use crate::{
    global::{AppAction, AppContext, AppEvent},
    input::{Counter, Cursor, GraphEditorCanvas, Slider},
    sequencer::{PlaybackContext, Sequencer},
    sound::{Beats, FromBeats, Note, Secs},
    visual::{GraphEditor, GraphPoint},
};
use std::{
    cmp::Ordering,
    mem::replace,
    num::NonZeroUsize,
    ops::{Range, RangeInclusive},
};
use wasm_bindgen::JsCast;
use wavexp_utils::{
    cell::{Shared, SharedAwareRefMut},
    default, js_function, r32, r64, AppError, AppResult, AppResultUtils, ArrayExt, ArrayFrom,
    OptionExt, RangeExt, R32, R64,
};
use web_sys::{AudioNode, Path2d};
use yew::{html, Html};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NoteBlock {
    pub offset: Beats,
    pub value: Note,
    pub len: Beats,
}

impl PartialOrd for NoteBlock {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.offset.cmp(&other.offset))
    }
}

impl Ord for NoteBlock {
    fn cmp(&self, other: &Self) -> Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl GraphPoint for NoteBlock {
    const EDITOR_NAME: &'static str = "Note Editor";
    const Y_BOUND: Range<R64> = r64![0]..r64![Note::N_NOTES];
    const SCALE_Y_BOUND: Range<R64> = {
        let x = r64![10 - (Note::N_NOTES - 1) % 10 + Note::N_NOTES - 1];
        x..x
    };
    const OFFSET_Y_BOUND: Range<R64> = {
        let x = r64![(10 - Note::N_NOTES as i8 % 10) / -2];
        x..x
    };
    const Y_SNAP: R64 = r64![1];

    type Inner = Beats;
    type Y = Note;
    /// (sound block offset, number of repetitions of the pattern)
    type VisualContext = (Beats, NonZeroUsize);

    fn create(_: &GraphEditor<Self>, [offset, y]: [R64; 2]) -> Self {
        Self {
            offset,
            value: Note::from_index(y.into()).recip(),
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
        &self.value
    }
    fn y_mut(&mut self) -> &mut Self::Y {
        &mut self.value
    }

    fn loc(&self) -> [R64; 2] {
        [self.offset, self.value.recip().index().into()]
    }

    fn move_point(point: Result<&mut Self, &mut [R64; 2]>, delta: [R64; 2], meta: bool) {
        match point {
            Ok(NoteBlock { offset, value, len }) => {
                if meta {
                    *len += delta[0];
                } else {
                    *offset += delta[0];
                    *offset = R64::ZERO.max(*offset);
                }
                *value -= delta[1].into();
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
            .contains(&self.value.recip().index())
            && (self.offset..=self.offset + self.len).overlap(&area[0]))
    }

    fn fmt_loc(loc: [R64; 2]) -> String {
        format!("{:.3}, {}", loc[0], Note::from_index(loc[1].into()).recip())
    }

    fn on_move(
        editor: &mut GraphEditor<Self>,
        ctx: &mut AppContext,
        _: Cursor,
        _: [R64; 2],
        point: Option<usize>,
    ) -> AppResult<()> {
        let Some(last) = editor.len().checked_sub(1) else {
            return Ok(());
        };
        Ok(
            if point.map_or_else(|| editor.selection().contains(&last), |x| x == last) {
                ctx.emit_event(AppEvent::RedrawEditorPlane)
            },
        )
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
        let step = &canvas_size.div(&editor.scale());
        let offset = &R64::array_from(editor.offset());
        for block in editor.iter() {
            let [x, y] = block.loc().mul(step).sub(offset);
            solid.rect(*x, *y, *block.len * *step[0], *step[1]);
        }

        let total_len = editor.last().map_or_default(|x| x.offset + x.len);
        Ok(if let PlaybackContext::All(start) = sequencer.playback_ctx() && start.is_finite() {
            let progress = (ctx.frame() - start).secs_to_beats(sequencer.bps()) - sb_offset;
            if progress < total_len * n_reps {
                editor.force_redraw();
                let x = R64::new_or(progress, *progress % *total_len) * step[0] - offset[0];
                solid.move_to(*x, 0.0);
                solid.line_to(*x, *canvas_size[1]);
            }
        })
    }
}

#[derive(Debug, Clone)]
pub struct NoteSound {
    pattern: Shared<GraphEditor<NoteBlock>>,
    volume: R32,
    attack: Beats,
    decay: Beats,
    sustain: R32,
    release: Beats,
    rep_count: NonZeroUsize,
}

impl Default for NoteSound {
    fn default() -> Self {
        Self {
            pattern: default(),
            volume: r32![1],
            attack: r64![0],
            decay: r64![0],
            sustain: r32![1],
            release: r64![0],
            rep_count: NonZeroUsize::MIN,
        }
    }
}

impl NoteSound {
    pub const NAME: &'static str = "Simple Wave";

    pub fn prepare(&self) -> AppResult<()> {
        Ok(())
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
            for NoteBlock { offset, value, len } in pat.iter() {
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

                let block_core = ctx.create_oscillator()?;
                block_core.frequency().set_value(*value.freq());
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
                <Slider key="note-vol"
                setter={emitter.reform(|x| AppEvent::Volume(R32::from(x)))}
                name="Note Volume"
                initial={self.volume}/>
                <Counter key="note-repcnt"
                setter={emitter.reform(|x| AppEvent::RepCount(NonZeroUsize::from(x)))}
                fmt={|x| format!("{x:.0}")}
                name="Number Of Pattern Repetitions"
                min={r64![1]}
                initial={self.rep_count}/>
            </div>},

            1 /* Envelope */ => html!{<div id="inputs">
                <Counter key="note-att"
                setter={emitter.reform(AppEvent::Attack)}
                name="Noise Attack Time" postfix="Beats"
                initial={self.attack}/>
                <Counter key="note-dec"
                setter={emitter.reform(AppEvent::Decay)}
                name="Note Decay Time" postfix="Beats"
                initial={self.decay}/>
                <Slider key="note-sus"
                setter={emitter.reform(|x| AppEvent::Sustain(R32::from(x)))}
                name="Note Sustain Level"
                initial={self.sustain}/>
                <Counter key="note-rel"
                setter={emitter.reform(AppEvent::Release)}
                name="Note Release Time" postfix="Beats"
                initial={self.release}/>
            </div>},

            2 /* Pattern */ => html!{
                <GraphEditorCanvas<NoteBlock> editor={&self.pattern} {emitter}/>
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
