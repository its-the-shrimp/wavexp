use std::{
    ops::{Deref, DerefMut, Range, Not},
    cmp::Ordering,
    cell::LazyCell,
    borrow::Cow,
    iter::once};
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::spawn_local;
use wavexp_utils::{
    R64,
    r64,
    AppResult,
    default,
    Pipe,
    ArrayExt,
    ArrayFrom,
    document,
    OptionExt,
    window,
    Check,
    R32, AppResultUtils};
use web_sys::{
    Path2d,
    HtmlCanvasElement,
    BaseAudioContext,
    AnalyserNode,
    GainNode,
    OfflineAudioContext,
    AudioContext};
use yew::{
    scheduler::Shared,
    Callback,
    Html,
    html};
use crate::{
    sound::{Sound, Beats, Secs, FromBeats},
    visual::{GraphPoint, GraphEditor},
    global::{AppContext, AppAction, AppEvent},
    input::{Cursor, Buttons, Slider}, time_stretcher::TimeStretcherNode};

#[derive(Debug, Clone)]
pub struct SoundBlock {
    pub sound: Sound,
    pub layer: i32,
    pub offset: Beats
}

impl Deref for SoundBlock {
    type Target = Sound;
    #[inline] fn deref(&self) -> &Self::Target {&self.sound}
}

impl DerefMut for SoundBlock {
    #[inline] fn deref_mut(&mut self) -> &mut Self::Target {&mut self.sound}
}

impl PartialEq for SoundBlock {
    #[inline] fn eq(&self, other: &Self) -> bool {
        self.offset.eq(&other.offset)
    }
}

impl Eq for SoundBlock {}

impl PartialOrd for SoundBlock {
    #[inline] fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.offset.cmp(&other.offset))
    }
}

impl Ord for SoundBlock {
    #[inline] fn cmp(&self, other: &Self) -> Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl GraphPoint for SoundBlock {
    const EDITOR_NAME: &'static str = "Editor plane";
    const Y_BOUND: Range<R64> = r64![0.0] .. R64::INFINITY;
    const SCALE_Y_BOUND: Range<R64> = r64![5.0] .. r64![30.0];
    const OFFSET_Y_BOUND: Range<R64> = r64![-1.0] .. R64::INFINITY;
    const Y_SNAP: R64 = r64![1.0];
    type Inner = Sound;
    type Y = i32;
    type VisualContext = ();

    #[inline] fn inner(&self) -> &Self::Inner {&self.sound}
    #[inline] fn inner_mut(&mut self) -> &mut Self::Inner {&mut self.sound}

    #[inline] fn y(&self) -> &Self::Y {&self.layer}
    #[inline] fn y_mut(&mut self) -> &mut Self::Y {&mut self.layer}

    #[inline] fn loc(&self) -> [R64; 2] {[self.offset, self.layer.into()]}

    #[inline] fn move_point(point: Result<&mut Self, &mut [R64; 2]>, delta: [R64; 2], _: bool) {
         match point {
            Ok(SoundBlock{layer, offset, ..}) => {
                *offset = r64![0.0].max(*offset + delta[0]);
                *layer += i32::from(delta[1]);
            }
            Err(point) => {
                point[0] = r64![0.0].max(point[0] + delta[0]);
                point[1] += delta[1];
            }
        }
    }

    #[inline] fn in_hitbox(
        &self,
        point:     [R64; 2],
        _:         &AppContext,
        sequencer: &Sequencer,
        _:         Self::VisualContext
    ) -> AppResult<bool> {
        Ok(self.layer == *point[1] as i32
            && (self.offset .. self.offset + self.sound.len(sequencer.bps())?.max(r64![0.1]))
                .contains(&point[0]))
    }

    #[inline] fn fmt_loc(loc: [R64; 2]) -> String {
        format!("{:.3}, layer {}", loc[0], loc[1].floor())
    }

    #[inline] fn on_click(
        editor:        &mut GraphEditor<Self>,
        ctx:           &mut AppContext,
        cursor:        Cursor,
        pressed_at:    impl Deref<Target = [R64; 2]>,
        released_at:   impl Deref<Target = [R64; 2]>,
        old_selection: Option<&[usize]>
    ) -> AppResult<Option<AppAction>> {
        let sel_is_empty = LazyCell::new(|| editor.selection().is_empty());
        if cursor.meta
        && *sel_is_empty
        && old_selection.map_or(true, |x| x.is_empty())
        && *pressed_at == *released_at {
            let [offset, y] = *released_at;
            let layer = y.into();
            let block_id = editor.add_point(SoundBlock{sound: default(), layer, offset});
            Ok(Some(AppAction::AddSoundBlock{block_id, offset, layer}))
        } else {
            ctx.emit_event(AppEvent::Select(sel_is_empty.not().then_some(0)));
            Ok(None)
        }
    }

    #[inline] fn on_plane_hover(
        editor: &mut GraphEditor<Self>,
        ctx:    &mut AppContext,
        cursor: Cursor,
        _:      impl Deref<Target = [R64; 2]>,
        first:  bool
    ) -> AppResult<()> {
        if !first && Some(&*cursor) == editor.last_cursor().as_deref() {return Ok(())}
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
                "Click to add block, hold & drag to select".into()],
            Buttons{left: true, meta: false, ..} => 
                [Cow::from(Self::EDITOR_NAME) + ": Moving",
                "Release to stop".into()],
            Buttons{left: true, meta: true, ..} =>
                [Cow::from(Self::EDITOR_NAME) + ": Selecting",
                "Release to select".into()]
        };
        Ok(ctx.emit_event(AppEvent::SetHint(m, a)))
    }

    #[inline] fn on_point_hover(
        editor:   &mut GraphEditor<Self>,
        ctx:      &mut AppContext,
        cursor:   Cursor,
        point_id: usize,
        first:    bool
    ) -> AppResult<()> {
        let m = || unsafe{editor.get_unchecked(point_id)}.desc().into();
        let [m, a] = if cursor.left {
            [m() + ": moving", "Release to stop".into()]
        } else if first {
            [m(), "Hold & drag to move".into()]
        } else {return Ok(())};
        Ok(ctx.emit_event(AppEvent::SetHint(m, a)))
    }

    #[inline] fn on_selection_hover(
        editor: &mut GraphEditor<Self>,
        ctx:    &mut AppContext,
        cursor: Cursor,
        first:  bool
    ) -> AppResult<()> {
        if !first {return Ok(())}
        let m = editor.selection().len().pipe(|l| format!("{l} block{}", if l == 1 {""} else {"s"}));
        let [m, a] = if cursor.left {
            [(m + ": moving").into(), "Release to stop".into()]
        } else {
            [m.into(), "Hold & drag to move".into()]
        };
        Ok(ctx.emit_event(AppEvent::SetHint(m, a)))
    }

    #[inline] fn on_undo(
        editor: &mut GraphEditor<Self>,
        _:      &mut AppContext,
        action: &AppAction
    ) -> AppResult<()> {
        if let &AppAction::AddSoundBlock{block_id, ..} = action {
            editor.remove_points(once(block_id), drop)
        } else {Ok(())}
    }

    #[inline] fn on_redo(
        editor: &mut GraphEditor<Self>,
        _:      &mut AppContext,
        action: &AppAction
    ) -> AppResult<()> {
        Ok(if let &AppAction::AddSoundBlock{offset, layer, block_id} = action {
            unsafe{editor.insert_point(block_id, SoundBlock{sound: default(), layer, offset})}
        })
    }

    fn on_redraw(
        editor:      &mut GraphEditor<Self>,
        ctx:         &AppContext,
        sequencer:   &Sequencer,
        canvas_size: &[R64; 2],
        solid:       &Path2d,
        dotted:      &Path2d,
        _:           Self::VisualContext
    ) -> AppResult<()> {
        let step = &canvas_size.div(&editor.scale());
        let offset = &R64::array_from(editor.offset());
        let bps = sequencer.bps();
        for block in editor.iter() {
            let [mut x, y] = block.loc().mul(step).sub(offset).map(|x| *x);
            let n_reps = block.rep_count().get();
            let w = *block.len(bps)? * *step[0];
            solid.rect(x, y, w, *step[1]);
            for _ in 1 .. n_reps {
                x += w;
                dotted.rect(x, y, w, *step[1])
            }
        }

        Ok(if let Ok(start) = sequencer.playing_since().check(R64::is_finite) {
            editor.force_redraw();
            let x = (ctx.now() - start).secs_to_beats(bps) * step[0] - offset[0];
            solid.move_to(*x, 0.0);
            solid.line_to(*x, *canvas_size[1]);
        })
    }

    #[inline] fn canvas_coords(canvas: &HtmlCanvasElement) -> AppResult<[u32; 2]> {
        let doc = document();
        let w = doc.body().to_app_result()?.client_width()
            - canvas.previous_element_sibling().to_app_result()?
            .client_width();
        let h = canvas.client_height();
        Ok([w as u32, h as u32])
    }
}

impl SoundBlock {
    #[inline] pub fn desc(&self) -> String {
        format!("{} @ {}", self.sound.name(), Self::fmt_loc(self.loc()))
    }
}

pub struct Sequencer {
    pattern: Shared<GraphEditor<SoundBlock>>,
    audio_ctx: BaseAudioContext,
    analyser: AnalyserNode,
    gain: GainNode,
    ctx_created_at: Secs,
    bps: Beats,
    playing_since: Secs
}

impl Sequencer {
    pub const SAMPLE_RATE: u32 = 44100;
    pub const CHANNEL_COUNT: u32 = 2;

    pub fn new() -> AppResult<Self> {
        let audio_ctx = OfflineAudioContext::new_with_number_of_channels_and_length_and_sample_rate(
            Self::CHANNEL_COUNT,
            1,
            Self::SAMPLE_RATE as f32)?;
        let gain = audio_ctx.create_gain()?;
        gain.gain().set_value(0.2);
        Ok(Self{
            pattern: default(),
            analyser: audio_ctx.create_analyser()?,
            gain,
            audio_ctx: audio_ctx.into(),
            ctx_created_at: unsafe{R64::new_unchecked(window().performance().to_app_result()?.now())}
                / 1000,
            bps: r64![2.0],
            playing_since: Secs::NEG_INFINITY})
    }

    /// returns `R64::NEG_INFINITY` if no sound's playing at the moment
    #[inline] pub fn playing_since(&self) -> Secs {
        self.playing_since
    }

    #[inline] pub fn bps(&self) -> Beats {
        self.bps
    }

    #[inline] pub fn pattern(&self) -> &Shared<GraphEditor<SoundBlock>> {
        &self.pattern
    }

    #[inline] pub fn audio_ctx(&self) -> &BaseAudioContext {
        &self.audio_ctx
    }

    #[inline] pub fn analyser(&self) -> &AnalyserNode {
        &self.analyser
    }

    #[inline] pub fn volume(&self) -> R32 {
        unsafe{R32::new_unchecked(self.gain.gain().value())}
    }

    #[inline] pub fn params(&self, emitter: Callback<AppEvent>) -> Html {
        html!{
            <div id="inputs">
                <Slider key="tmp" name="Tempo"
                setter={emitter.reform(AppEvent::Bpm)}
                min={r64![30.0]} max={r64![240.0]}
                postfix="BPM"
                initial={self.bps * 60}/>
                <Slider key="gain" name="Master volume"
                setter={emitter.reform(|x| AppEvent::MasterVolume(R32::from(x)))}
                initial={self.volume()}/>
            </div>
        }
    }

    pub fn handle_event(&mut self, event: &AppEvent, ctx: &mut AppContext) -> AppResult<()> {
        Ok(match event {
            AppEvent::PreparePlay => if self.audio_ctx.is_instance_of::<AudioContext>() {
                ctx.emit_event(AppEvent::StartPlay)
            } else {
                let volume = self.volume();
                self.audio_ctx = AudioContext::new()?.into();
                self.analyser = self.audio_ctx.create_analyser()?;
                self.gain = self.audio_ctx.create_gain()?;
                self.gain.gain().set_value(*volume);
                self.gain.connect_with_audio_node(&self.analyser)?
                    .connect_with_audio_node(&self.audio_ctx.destination())?;
                let perf = window().performance().to_app_result()?;
                self.ctx_created_at = unsafe{R64::new_unchecked(perf.now()) / 1000};

                let audio_ctx = self.audio_ctx.clone();
                let emitter = ctx.event_emitter().clone();
                spawn_local(async move {
                    let res: AppResult<!> = try {
                        TimeStretcherNode::register(audio_ctx.audio_worklet()?).await?;
                        return emitter.emit(AppEvent::StartPlay)
                    };
                    res.report();
                });
            }

            AppEvent::StartPlay => {
                let perf = window().performance().to_app_result()?;
                let now = unsafe{R64::new_unchecked(perf.now()) / 1000 - self.ctx_created_at};
                let mut pattern = self.pattern.try_borrow_mut()?;
                for mut block in pattern.iter_mut() {
                    let offset = block.offset.to_secs(self.bps);
                    block.inner().play(&self.gain, now, offset, self.bps)?;
                }
                self.playing_since = now + self.ctx_created_at;
            }

            &AppEvent::MasterVolume(to) => unsafe {
                let gain = self.gain.gain();
                ctx.register_action(AppAction::SetMasterVolume{from: R32::new_unchecked(gain.value()), to});
                gain.set_value(*to);
            }

            &AppEvent::Bpm(mut to) => {
                to /= 60;
                ctx.register_action(AppAction::SetTempo{from: self.bps, to}); 
                self.bps = to
            }

            AppEvent::StopPlay => self.playing_since = R64::NEG_INFINITY,

            AppEvent::RedrawEditorPlane => self.pattern.try_borrow_mut()?
                .force_redraw(),

            AppEvent::Undo(actions) => for action in actions.iter() {
                match *action {
                    AppAction::SetTempo{from, ..} =>
                        self.bps = from,

                    AppAction::SetMasterVolume{from, ..} =>
                        self.gain.gain().set_value(*from),

                    _ => ()
                }
            }

            AppEvent::Redo(actions) => for action in actions.iter() {
                match *action {
                    AppAction::SetTempo{to, ..} =>
                        self.bps = to,

                    AppAction::SetMasterVolume{to, ..} =>
                        self.gain.gain().set_value(*to),

                    _ => ()
                }
            }

            e => self.pattern.try_borrow_mut()?.handle_event(e, ctx, self, || ())?
        })
    }
}
