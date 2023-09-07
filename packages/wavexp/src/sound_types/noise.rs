use std::{
    ops::{Range, Deref},
    num::NonZeroUsize,
    iter::once, mem::replace, borrow::Cow};
use js_sys::Math::random;
use wavexp_utils::{
    AppResult,
    ArrayFrom,
    ArrayExt,
    OptionExt,
    R64, r64,
    R32, r32,
    default,
    Pipe,
    js_function,
    AppError,
    AppResultUtils, cell::{Shared, SharedAwareRefMut}, SliceExt};
use web_sys::{Path2d, AudioBuffer, AudioBufferOptions, AudioNode};
use yew::{Html, html};
use wasm_bindgen::JsCast;
use crate::{
    sound::{Beats, Note, FromBeats, Secs},
    visual::{GraphPoint, GraphEditor},
    global::{AppContext, AppAction, AppEvent},
    sequencer::{Sequencer, PlaybackContext},
    input::{Slider, Counter, GraphEditorCanvas, Buttons, Cursor}};
use super::CustomBlock;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NoiseBlock {
    offset: Beats,
    pitch: Note,
    len: Beats
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

    fn inner(&self) -> &Self::Inner {&self.len}
    fn inner_mut(&mut self) -> &mut Self::Inner {&mut self.len}
    fn y(&self) -> &Self::Y {&self.pitch}
    fn y_mut(&mut self) -> &mut Self::Y {&mut self.pitch}
    fn loc(&self) -> [R64; 2] {
        [self.offset, self.pitch.recip().index().into()]
    }

    fn move_point(point: Result<&mut Self, &mut [R64; 2]>, delta: [R64; 2], meta: bool) {
        match point {
            Ok(NoiseBlock{offset, pitch, len}) => {
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
        point: [R64; 2],
        _:     &AppContext,
        _:     &Sequencer,
        _:     Self::VisualContext
    ) -> AppResult<bool> {
        Ok(self.pitch.recip().index() == *point[1] as usize
            && (self.offset .. self.offset + self.len).contains(&point[0]))
    }

    fn on_redraw(
        editor:              &mut GraphEditor<Self>,
        ctx:                 &AppContext,
        sequencer:           &Sequencer,
        canvas_size:         &[R64; 2],
        solid:               &Path2d,
        _:                   &Path2d,
        (sb_offset, n_reps): Self::VisualContext
    ) -> AppResult<()> {
        let bps = sequencer.bps();
        let step = &canvas_size.div(&editor.scale());
        let offset = &R64::array_from(editor.offset());
        for block in editor.iter() {
            let [x, y] = block.loc().mul(step).sub(offset);
            solid.rect(*x, *y, *block.len / *block.pitch.pitch_coef() * *step[0], *step[1]);
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

    fn on_plane_hover(
        editor: &mut GraphEditor<Self>,
        ctx:    &mut AppContext,
        cursor: Cursor,
        _:      impl Deref<Target = [R64; 2]>,
        first:  bool
    ) -> AppResult<()> {
        if !first && editor.last_cursor().is_some_and(|x| *x == *cursor) {return Ok(())}
        let [m, a] = match *cursor {
            Buttons{left: false, shift: true, ..} =>
                [Self::EDITOR_NAME.into(),
                "Press and hold to zoom".into()],
            Buttons{left: true, shift: true, ..} =>
                [Cow::from(Self::EDITOR_NAME) + ": zooming",
                "Release to stop".into()],
            Buttons{left: false, meta: false, ..} =>
                [Self::EDITOR_NAME.into(),
                "Hold & drag to move around (press Meta for actions)".into()],
            Buttons{left: false, meta: true, ..} =>
                [Self::EDITOR_NAME.into(),
                "Click to add note, hold & drag to select".into()],
            Buttons{left: true, meta: false, ..} => 
                [Cow::from(Self::EDITOR_NAME) + ": Moving",
                "Release to stop".into()],
            Buttons{left: true, meta: true, ..} =>
                [Cow::from(Self::EDITOR_NAME) + ": Selecting",
                "Release to select".into()]
        };
        Ok(ctx.emit_event(AppEvent::SetHint(m, a)))
    }

    fn on_point_hover(
        editor:   &mut GraphEditor<Self>,
        ctx:      &mut AppContext,
        cursor:   Cursor,
        point_id: usize,
        first:    bool
    ) -> AppResult<()> {
        if !first && editor.last_cursor().is_some_and(|x| x.left == cursor.left) {return Ok(())}
        let m = Self::fmt_loc(unsafe{editor.get_unchecked(point_id)}.loc());
        let [m, a] = match *cursor {
            Buttons{left: false, ..} =>
                [m.into(), "LMB to move, LMB + Meta to stretch".into()],
            Buttons{left: true, meta: false, ..} =>
                [(m + ": Moving").into(), "Release to stop".into()],
            Buttons{left: true, meta: true, ..} =>
                [(m + ": Stretching").into(), "Release to stop".into()],
        };
        Ok(ctx.emit_event(AppEvent::SetHint(m, a)))
    }

    fn on_selection_hover(
        editor: &mut GraphEditor<Self>,
        ctx:    &mut AppContext,
        cursor: Cursor,
        first:  bool
    ) -> AppResult<()> {
        if !first && editor.last_cursor().is_some_and(|x| x.left == cursor.left) {return Ok(())}
        let m = {let n = editor.selection().len(); format!("{n} block{}", if n == 1 {""} else {"s"})};
        let [m, a] = match *cursor {
            Buttons{left: false, ..} =>
                [m.into(), "LMB to move, LMB + Meta to stretch".into()],
            Buttons{left: true, meta: false, ..} =>
                [(m + ": Moving").into(), "Release to stop".into()],
            Buttons{left: true, meta: true, ..} =>
                [(m + ": Stretching").into(), "Release to stop".into()],
        };
        Ok(ctx.emit_event(AppEvent::SetHint(m, a)))
    }


    fn fmt_loc(loc: [R64; 2]) -> String {CustomBlock::fmt_loc(loc)}

    fn on_click(
        editor:        &mut GraphEditor<Self>,
        ctx:           &mut AppContext,
        cursor:        Cursor,
        pressed_at:    impl Deref<Target = [R64; 2]>,
        released_at:   impl Deref<Target = [R64; 2]>,
        old_selection: Option<&[usize]>
    ) -> AppResult<Option<AppAction>> {
        if !cursor.meta {return Ok(None)}

        let delta = released_at.sub(&pressed_at);
        Ok(if delta.all(|x| *x == 0) {
            if !editor.selection().is_empty() || old_selection.map_or(false, |x| !x.is_empty()) {
                return Ok(None)
            }
            let [offset, y] = *released_at;
            let pitch = Note::from_index(y.into()).recip();
            let block_id = editor.add_point(Self{offset, pitch, len: r64![1]});
            ctx.emit_event(AppEvent::RedrawEditorPlane);
            Some(AppAction::AddNoiseBlock{block_id, offset, pitch})
        } else {
            ctx.emit_event(AppEvent::RedrawEditorPlane);
            let mut removed = Vec::with_capacity(editor.selection().len());
            let (delta_x, delta_y) = (delta[0], isize::from(delta[1]));
            editor.filter_selected(|x| x.1.len > 0, |mut x| {
                x.1.len -= delta_x;
                x.1.pitch += delta_y;
                removed.push(x)
            });
            match removed.len() {
                0 => None,
                n if n == removed.capacity() => Some(AppAction::RemoveNoiseBlocks(removed.into_boxed_slice())),
                _ => Some(AppAction::StretchNoiseBlocks{delta_x, delta_y, removed: removed.into_boxed_slice()})
            }
        })
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
    rep_count: NonZeroUsize
}

impl NoiseSound {
    pub const NAME: &str = "White Noise";

    pub fn prepare(&self) -> AppResult<()> {Ok(())}

    pub fn new() -> AppResult<Self> {
        let mut buf = vec![0.0; Sequencer::SAMPLE_RATE as usize]; // 1 second of noise
        buf.fill_with(|| random() as f32 * 2.0 - 1.0);
        let src = AudioBufferOptions::new(Sequencer::SAMPLE_RATE, Sequencer::SAMPLE_RATE as f32)
            .number_of_channels(Sequencer::CHANNEL_COUNT)
            .pipe(|x| AudioBuffer::new(x))?;
        for i in 0 .. Sequencer::CHANNEL_COUNT as i32 {
            src.copy_to_channel(&buf, i)?;
        }
        Ok(Self{pattern: default(), src,
            volume: r32![0.2], attack: r64![0], decay: r64![0], sustain: r32![1], release: r64![0.2],
            rep_count: NonZeroUsize::MIN})
    }

    pub fn play(&mut self, plug: &AudioNode, now: Secs, self_offset: Secs, bps: Beats) -> AppResult<()> {
        let pat = self.pattern.get()?;
        let Some(last) = pat.last() else {return Ok(())};
        let pat_len = (last.offset + last.len / last.pitch.pitch_coef()).to_secs(bps);
        let ctx = plug.context();

        Ok(for rep in 0 .. self.rep_count.get() {
            for NoiseBlock{offset, len, pitch} in pat.iter() {
                let block = ctx.create_gain()?;
                let gain = block.gain();
                let start = now + self_offset + pat_len * rep + offset.to_secs(bps);
                let mut at = start;
                gain.set_value(0.0);
                at += self.attack.to_secs(bps);
                gain.linear_ramp_to_value_at_time(*self.volume, *at)?;
                at += self.decay.to_secs(bps);
                self.sustain *= self.volume;
                gain.linear_ramp_to_value_at_time(*self.sustain, *at)?;
                at = start + len.to_secs(bps) / pitch.pitch_coef();
                gain.set_value_at_time(*self.sustain, *at)?;
                at += self.release.to_secs(bps);
                gain.linear_ramp_to_value_at_time(0.0, *at)?;

                let block_core = ctx.create_buffer_source()?;
                block_core.set_buffer(Some(&self.src));
                if at - start > 1 {
                    block_core.set_loop(true);
                }
                block_core.connect_with_audio_node(&block)?
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
        Ok(self.pattern.get()?
            .last().map_or_default(|x| x.offset + x.len / x.pitch.pitch_coef()))
    }

    pub fn rep_count(&self) -> NonZeroUsize {self.rep_count}

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
        offset: Beats
    ) -> AppResult<()> {
        Ok(match *event {
            AppEvent::Volume(to) =>
                ctx.register_action(AppAction::SetVolume{from: replace(&mut self.volume, to), to}),

            AppEvent::Attack(to) =>
                ctx.register_action(AppAction::SetAttack{from: replace(&mut self.attack, to), to}),

            AppEvent::Decay(to) =>
                ctx.register_action(AppAction::SetDecay{from: replace(&mut self.decay, to), to}),

            AppEvent::Sustain(to) =>
                ctx.register_action(AppAction::SetSustain{from: replace(&mut self.sustain, to), to}),

            AppEvent::Release(to) =>
                ctx.register_action(AppAction::SetRelease{from: replace(&mut self.release, to), to}),

            AppEvent::RepCount(to) => {
                ctx.register_action(AppAction::SetRepCount{from: replace(&mut self.rep_count, to), to});
                ctx.emit_event(AppEvent::RedrawEditorPlane);
            }

            AppEvent::Undo(ref actions) => {
                let mut pat = self.pattern.get_mut()?;
                for action in actions.iter() {
                    match *action {
                        AppAction::SetBlockType(_) => {
                            *reset_sound = true;
                            break
                        }

                        AppAction::SetVolume{from, ..} =>
                            self.volume = from,

                        AppAction::SetAttack{from, ..} =>
                            self.attack = from,

                        AppAction::SetDecay{from, ..} =>
                            self.decay = from,

                        AppAction::SetSustain{from, ..} =>
                            self.sustain = from,

                        AppAction::SetRelease{from, ..} =>
                            self.release = from,

                        AppAction::SetRepCount{from, ..} => {
                            self.rep_count = from;
                            ctx.emit_event(AppEvent::RedrawEditorPlane)
                        }

                        AppAction::AddNoiseBlock{block_id, ..} => {
                            pat.remove_points(once(block_id), drop)?;
                            ctx.emit_event(AppEvent::RedrawEditorPlane)
                        }

                        AppAction::RemoveNoiseBlocks(ref blocks) => {
                            for &(at, b) in blocks.as_ref() {
                                unsafe{pat.insert_point(at, b)}
                            }
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
                        AppAction::SetVolume{to, ..} =>
                            self.volume = to,

                        AppAction::SetAttack{to, ..} =>
                            self.attack = to,

                        AppAction::SetDecay{to, ..} =>
                            self.decay = to,

                        AppAction::SetSustain{to, ..} =>
                            self.sustain = to,

                        AppAction::SetRelease{to, ..} =>
                            self.release = to,

                        AppAction::SetRepCount{to, ..} => {
                            self.rep_count = to;
                            ctx.emit_event(AppEvent::RedrawEditorPlane)
                        }

                        AppAction::AddNoiseBlock{block_id, offset, pitch} => {
                            unsafe{pat.insert_point(block_id, NoiseBlock{offset, pitch, len: r64![1]})};
                            ctx.emit_event(AppEvent::RedrawEditorPlane)
                        }

                        AppAction::RemoveNoiseBlocks(ref removed) => {
                            pat.remove_points(removed.iter().map(|(id, _)| *id), drop)?;
                            ctx.emit_event(AppEvent::RedrawEditorPlane)
                        }

                        _ => (),
                    }
                }

                if ctx.selected_tab() == 2 {
                    pat.handle_event(event, ctx, sequencer, || (offset, self.rep_count))?;
                }
            }

            _ => if ctx.selected_tab() == 2 {
                self.pattern.get_mut()?
                    .handle_event(event, ctx, sequencer, || (offset, self.rep_count))?;
            }
        })
    }
}
