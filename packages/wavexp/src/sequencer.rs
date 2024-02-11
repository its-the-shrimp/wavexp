use crate::{
    ctx::{AppEvent, ContextMut, ContextRef, EditorAction},
    img,
    input::{AudioInputButton, Button, Slider, Tab},
    popup::{ExportFormat, Popup},
    sound::{AudioInput, Beats, FromBeats, Secs, Sound},
    visual::{GraphEditor, GraphPoint},
};
use macro_rules_attribute::apply;
use std::{
    cmp::Ordering,
    fmt::{self, Display, Formatter},
    ops::{Deref, DerefMut, RangeBounds},
};
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::spawn_local;
use wavexp_utils::{
    cell::Shared,
    error::Result,
    ext::default,
    ext::{ArrayExt, ResultExt, SliceExt},
    fallible,
    js::save_file,
    js::{document, now},
    js_function, r64,
    range::{RangeBoundsExt, RangeInclusiveV2, RangeV2},
    real::R32,
    real::R64,
    ArrayFrom,
};
use web_sys::{
    AnalyserNode, AudioContext, BaseAudioContext, GainNode, HtmlCanvasElement, HtmlInputElement,
    OfflineAudioContext, Path2d,
};
use yew::{html, AttrValue, Html, TargetCast};

#[derive(Debug, Clone)]
pub struct SoundBlock {
    pub sound: Sound,
    pub layer: u32,
    pub offset: Beats,
}

impl Deref for SoundBlock {
    type Target = Sound;
    fn deref(&self) -> &Self::Target {
        &self.sound
    }
}

impl DerefMut for SoundBlock {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.sound
    }
}

impl PartialEq for SoundBlock {
    fn eq(&self, other: &Self) -> bool {
        self.offset.eq(&other.offset)
    }
}

impl Eq for SoundBlock {}

impl PartialOrd for SoundBlock {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.offset.cmp(&other.offset))
    }
}

impl Ord for SoundBlock {
    fn cmp(&self, other: &Self) -> Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl GraphPoint for SoundBlock {
    const EDITOR_NAME: &'static str = "Editor plane";
    const Y_BOUND: RangeV2<R64> = RangeV2 { start: r64!(0), end: R64::INFINITY };
    const SCALE_Y_BOUND: RangeV2<R64> = RangeV2 { start: r64!(5), end: r64!(30) };
    const OFFSET_Y_BOUND: RangeV2<R64> = RangeV2 { start: r64!(-1), end: R64::INFINITY };
    const Y_SNAP: R64 = r64!(1);
    type Inner = Sound;
    type Y = u32;
    type VisualContext = ();

    fn create(_: &GraphEditor<Self>, [offset, y]: [R64; 2]) -> Self {
        Self { sound: default(), layer: y.into(), offset }
    }

    fn inner(&self) -> &Self::Inner {
        &self.sound
    }
    fn inner_mut(&mut self) -> &mut Self::Inner {
        &mut self.sound
    }

    fn y(&self) -> &Self::Y {
        &self.layer
    }
    fn y_mut(&mut self) -> &mut Self::Y {
        &mut self.layer
    }

    fn loc(&self) -> [R64; 2] {
        [self.offset, self.layer.into()]
    }

    fn móve(&mut self, delta: [R64; 2], _: bool) -> Result {
        self.offset = r64!(0).max(self.offset + delta[0]);
        self.layer += u32::from(delta[1]);
        Ok(())
    }

    fn move_point(point: &mut [R64; 2], delta: [R64; 2], _: bool) {
        point[0] = r64!(0).max(point[0] + delta[0]);
        point[1] += delta[1]
    }

    #[apply(fallible!)]
    fn in_hitbox(
        &self,
        area: &[RangeInclusiveV2<R64>; 2],
        _: ContextRef,
        sequencer: &Sequencer,
        _: Self::VisualContext,
    ) -> bool {
        area[1].map_bounds(u32::from).contains(&self.layer)
            && (self.offset..=self.sound.len(sequencer.bps())?.max(r64!(0.1)) + self.offset)
                .overlap(&area[0])
    }

    fn fmt_loc(loc: [R64; 2]) -> String {
        format!("{:.3}, layer {}", loc[0], loc[1].floor())
    }

    fn on_selection_change(editor: &mut GraphEditor<Self>, ctx: ContextMut) -> Result {
        Ok(ctx.emit_event(AppEvent::Select(editor.selection().not_empty().then_some(0))))
    }

    fn on_redraw(
        editor: &mut GraphEditor<Self>,
        ctx: ContextRef,
        sequencer: &Sequencer,
        canvas_size: &[R64; 2],
        solid: &Path2d,
        dotted: &Path2d,
        _: Self::VisualContext,
    ) -> Result {
        let step = canvas_size.div(editor.scale());
        let offset = R64::array_from(editor.offset());
        let bps = sequencer.bps();
        for block in editor.data() {
            let [mut x, y] = block.loc().mul(step).sub(offset).map(|x| *x);
            let n_reps = block.rep_count().get();
            let w = *block.len(bps)? * *step[0];
            solid.rect(x, y, w, *step[1]);
            for _ in 1..n_reps {
                x += w;
                dotted.rect(x, y, w, *step[1])
            }
        }

        if let PlaybackContext::All(start) = sequencer.playback_ctx() && start.is_finite() {
            editor.force_redraw();
            let x = (ctx.frame() - start).secs_to_beats(bps) * step[0] - offset[0];
            solid.move_to(*x, 0.0);
            solid.line_to(*x, *canvas_size[1]);
        }
        Ok(())
    }

    #[apply(fallible!)]
    fn canvas_coords(canvas: &HtmlCanvasElement) -> [u32; 2] {
        let doc = document();
        let w = doc.body()?.client_width() - canvas.previous_element_sibling()?.client_width();
        let h = canvas.client_height();
        [w as u32, h as u32]
    }
}

impl Display for SoundBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} @ {}", self.sound.name(), Self::fmt_loc(self.loc()))
    }
}

impl SoundBlock {
    pub fn tabs(&self, ctx: ContextRef) -> Html {
        let desc = &AttrValue::from(self.to_string() + ": Settings");
        match self.sound {
            Sound::None => html! { <Tab name="Choose Sound Type" {desc} selected=true /> },
            Sound::Note { .. } | Sound::Noise { .. } | Sound::Custom { .. } => {
                let setter = ctx.event_emitter().reform(AppEvent::SetTab);
                let id = ctx.selected_tab();
                html! {
                    <>
                        <Tab
                            name="General"
                            {desc}
                            setter={setter.reform(|_| 0)}
                            selected={id == 0}
                        />
                        <Tab
                            name="Envelope"
                            {desc}
                            setter={setter.reform(|_| 1)}
                            selected={id == 1}
                        />
                        <Tab
                            name="Pattern"
                            {desc}
                            setter={setter.reform(|_| 2)}
                            selected={id == 2}
                        />
                    </>
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum PlaybackContext {
    None,
    One(Shared<AudioInput>, Secs),
    All(Secs),
}

impl PlaybackContext {
    pub const fn playing(&self) -> bool {
        !matches!(self, Self::None)
    }

    pub const fn all_playing(&self) -> bool {
        matches!(self, Self::All(..))
    }

    pub const fn played_input(&self) -> Option<&Shared<AudioInput>> {
        if let Self::One(x, _) = self {
            Some(x)
        } else {
            None
        }
    }
}

pub struct Composition {
    pub pattern: Shared<GraphEditor<SoundBlock>>,
    pub inputs: Vec<Shared<AudioInput>>,
    pub bps: Beats,
}

impl Default for Composition {
    fn default() -> Self {
        Composition { bps: r64!(2), pattern: default(), inputs: vec![] }
    }
}

pub struct Sequencer {
    comp: Composition,
    audio_ctx: BaseAudioContext,
    analyser: AnalyserNode,
    gain: GainNode,
    ctx_created_at: Secs,
    playback_ctx: PlaybackContext,
}

impl Sequencer {
    pub const SAMPLE_RATE: u32 = 44100;
    pub const CHANNEL_COUNT: u32 = 2;

    #[apply(fallible!)]
    pub fn new() -> Self {
        let audio_ctx =
            OfflineAudioContext::new_with_number_of_channels_and_length_and_sample_rate(
                Self::CHANNEL_COUNT,
                1,
                Self::SAMPLE_RATE as f32,
            )?;
        let gain = audio_ctx.create_gain()?;
        gain.gain().set_value(0.2);
        Self {
            comp: default(),
            analyser: audio_ctx.create_analyser()?,
            gain,
            audio_ctx: audio_ctx.into(),
            ctx_created_at: now()? / 1000,
            playback_ctx: PlaybackContext::None,
        }
    }

    pub const fn bps(&self) -> Beats {
        self.comp.bps
    }
    pub const fn pattern(&self) -> &Shared<GraphEditor<SoundBlock>> {
        &self.comp.pattern
    }
    pub const fn audio_ctx(&self) -> &BaseAudioContext {
        &self.audio_ctx
    }
    pub const fn analyser(&self) -> &AnalyserNode {
        &self.analyser
    }
    pub const fn playback_ctx(&self) -> &PlaybackContext {
        &self.playback_ctx
    }

    pub fn volume(&self) -> R32 {
        unsafe { R32::new_unchecked(self.gain.gain().value()) }
    }

    pub fn inputs(&self) -> &[Shared<AudioInput>] {
        &self.comp.inputs
    }

    pub fn tabs(&self, ctx: ContextRef) -> Html {
        let id = ctx.selected_tab();
        let setter = ctx.event_emitter();
        html! {
            <>
                <Tab
                    name="General"
                    desc="General settings"
                    setter={setter.reform(|_| AppEvent::SetTab(0))}
                    selected={id == 0}
                />
                <Tab
                    name="Inputs"
                    desc="General settings"
                    setter={setter.reform(|_| AppEvent::SetTab(1))}
                    selected={id == 1}
                />
            </>
        }
    }

    pub fn params(&self, ctx: ContextRef) -> Html {
        let emitter = ctx.event_emitter();
        match ctx.selected_tab() {
            0 /* General */ => html! {
                <div id="inputs">
                    <Slider
                        key="tmp"
                        name="Tempo"
                        setter={emitter.reform(AppEvent::Bpm)}
                        min=30
                        max=240
                        postfix="BPM"
                        initial={self.comp.bps * 60}
                    />
                    <Slider
                        key="gain"
                        name="Master volume"
                        setter={emitter.reform(|x| AppEvent::MasterVolume(R32::from(x)))}
                        initial={self.volume()}
                    />
                    <div
                        class="export-options"
                    >
                        <Button
                            name="Export the project"
                            class="wide"
                            help="Save the whole project as an audio file"
                            onclick={emitter.reform(|_| {
                                AppEvent::OpenPopup(
                                    Popup::Export {
                                        format: ExportFormat::Wav,
                                        filename: "project.wav".into(),
                                        err_msg: default(),
                                    }
                                )
                            })}
                        >
                            <span>{ "Export the project" }</span>
                        </Button>
                        <Button
                            name="Save the project"
                            help="All the patterns & inputs will be saved as they are"
                            onclick={emitter.reform(|_| {
                                AppEvent::OpenPopup(
                                    Popup::Export {
                                        format: ExportFormat::Wavexp,
                                        filename: "project.wavexp".into(),
                                        err_msg: default(),
                                    }
                                )
                            })}
                        >
                            <img::FloppyDisk />
                        </Button>
                    </div>
                </div>
            },

            1 /* Inputs */ => html!{
                <div class="horizontal-menu dark-bg">
                    { for self.comp.inputs.iter().map(|input| html! {
                        <AudioInputButton
                            playing={self.playback_ctx.played_input().is_some_and(|i| i.eq(input))}
                            name={input.get().map_or_default(|x| AttrValue::from(x.name().clone()))}
                            {input}
                            {emitter}
                            bps={self.comp.bps}
                            class="extend-inner-button-panel"
                        />
                    }) }
                    <Button
                        name="Add audio input"
                        onclick={emitter.reform(|_| AppEvent::StartInputAdd)}
                    >
                        <img::Plus />
                    </Button>
                </div>
            },

            tab_id => html!(<p style="color:red">{ format!("Invalid tab ID: {tab_id}") }</p>)
        }
    }

    #[apply(fallible!)]
    pub fn handle_event(&mut self, event: &AppEvent, mut ctx: ContextMut) {
        match *event {
            AppEvent::PreparePlay(ref input) => {
                if self.audio_ctx.is_instance_of::<AudioContext>() {
                    self.playback_ctx = PlaybackContext::None;
                    self.gain.disconnect()?;
                } else {
                    self.audio_ctx = AudioContext::new()?.into();
                    self.analyser = self.audio_ctx.create_analyser()?;
                    self.analyser.connect_with_audio_node(&self.audio_ctx.destination())?;
                    self.ctx_created_at = now()?;
                }
                if let Some(input) = input {
                    input.get_mut()?.bake(self.comp.bps)?;
                } else {
                    for mut block in self.comp.pattern.get_mut()?.iter_data_mut() {
                        block.inner().prepare(self.comp.bps)?;
                    }
                }
                let volume = self.volume();
                self.gain = self.audio_ctx.create_gain()?;
                self.gain.gain().set_value(*volume);
                self.gain.connect_with_audio_node(&self.analyser)?;
                ctx.emit_event(AppEvent::StartPlay(input.clone()))
            }

            AppEvent::StartPlay(ref input) => {
                let now = now()? - self.ctx_created_at;
                if let Some(input) = input {
                    let player = self.audio_ctx.create_buffer_source()?;
                    {
                        let input = input.get()?;
                        player.set_buffer(Some(input.baked()?));
                        player.connect_with_audio_node(&self.gain)?;
                    }
                    self.playback_ctx =
                        PlaybackContext::One(input.clone(), now + self.ctx_created_at);
                    let emitter = ctx.event_emitter().clone();
                    player.set_onended(Some(&js_function!(|| emitter.emit(AppEvent::StopPlay))));
                    player.start()?;
                } else {
                    self.playback_ctx = PlaybackContext::All(now + self.ctx_created_at);
                    let mut pattern = self.comp.pattern.get_mut()?;
                    for mut block in pattern.iter_data_mut() {
                        let offset = block.offset.to_secs(self.comp.bps);
                        block.inner().play(&self.gain, now, offset, self.comp.bps)?;
                    }
                }
            }

            AppEvent::StopPlay => {
                self.playback_ctx = PlaybackContext::None;
                self.gain.disconnect()?;
            }

            AppEvent::StartInputAdd => {
                let temp = document().create_element("input")?.unchecked_into::<HtmlInputElement>();
                temp.set_type("file");
                let emitter = ctx.event_emitter().clone();
                temp.set_onchange(Some(
                    &js_function!(|e| emitter.emit(AppEvent::AudioUploaded(e))),
                ));
                temp.click();
            }

            AppEvent::Export(ref filename) => {
                let filename = filename.clone();
                let wav = self.comp.export(self.volume())?;
                spawn_local(async move {
                    let Some(wav) = wav.await.report() else {
                        return;
                    };
                    save_file(&wav, &filename).report();
                })
            }

            AppEvent::Save(ref filename) => save_file(&self.comp.encode()?, filename)?,

            AppEvent::AudioUploaded(ref e) => {
                let target: HtmlInputElement = e.target_dyn_into()?;
                let emitter = ctx.event_emitter().clone();

                let file = target.files().and_then(|x| x.get(0))?;
                let future_file = AudioInput::from_file(file, self);
                spawn_local(async move {
                    if let Some(input) = future_file.await.report() {
                        emitter.emit(AppEvent::AddInput(input.into()))
                    }
                })
            }

            AppEvent::AddInput(ref input) => {
                ctx.register_action(EditorAction::AddInput(input.clone()))?;
                self.comp.inputs.push(input.clone());
            }

            AppEvent::MasterVolume(to) => {
                let gain = self.gain.gain();
                ctx.register_action(EditorAction::SetMasterVolume {
                    from: R32::new(gain.value())?,
                    to,
                })?;
                gain.set_value(*to);
            }

            AppEvent::Bpm(mut to) => {
                to /= 60;
                ctx.register_action(EditorAction::SetTempo { from: self.comp.bps, to })?;
                self.comp.bps = to
            }

            AppEvent::RedrawEditorPlane => self.comp.pattern.get_mut()?.force_redraw(),

            AppEvent::Undo(ref actions) => {
                for action in actions.iter() {
                    match *action {
                        EditorAction::SetTempo { from, .. } => self.comp.bps = from,

                        EditorAction::SetMasterVolume { from, .. } => {
                            self.gain.gain().set_value(*from)
                        }

                        EditorAction::AddInput(_) => _ = self.comp.inputs.pop(),

                        _ => (),
                    }
                }
                self.comp.pattern.get_mut()?.handle_event(event, ctx, self, || ())?
            }

            AppEvent::Redo(ref actions) => {
                for action in actions.iter() {
                    match *action {
                        EditorAction::SetTempo { to, .. } => self.comp.bps = to,

                        EditorAction::SetMasterVolume { to, .. } => self.gain.gain().set_value(*to),

                        EditorAction::AddInput(ref input) => self.comp.inputs.push(input.clone()),

                        _ => (),
                    }
                }
                self.comp.pattern.get_mut()?.handle_event(event, ctx, self, || ())?
            }

            _ => self.comp.pattern.get_mut()?.handle_event(event, ctx, self, || ())?,
        }
    }
}
