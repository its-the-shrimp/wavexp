use std::{
    ops::{Deref, DerefMut, Range, Not, RangeInclusive},
    cmp::Ordering,
    fmt::{Display, Formatter, self},
    rc::Rc, mem::replace};
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::spawn_local;
use wavexp_utils::{
    R64,
    r64,
    AppResult,
    default,
    ArrayExt,
    ArrayFrom,
    document,
    OptionExt,
    R32,
    AppResultUtils,
    now,
    js_function,
    BoolExt,
    cell::{Shared, SharedAwareRefMut}, RangeExt};
use web_sys::{
    Path2d,
    HtmlCanvasElement,
    BaseAudioContext,
    AnalyserNode,
    GainNode,
    OfflineAudioContext,
    AudioContext,
    HtmlInputElement};
use yew::{
    Html,
    html,
    AttrValue,
    TargetCast};
use crate::{
    sound::{Sound, Beats, Secs, FromBeats, AudioInput},
    visual::{GraphPoint, GraphEditor},
    global::{AppContext, AppAction, AppEvent, Popup},
    input::{Slider, Tab, Button, AudioInputButton},
    img};

#[derive(Debug, Clone)]
pub struct SoundBlock {
    pub sound: Sound,
    pub layer: i32,
    pub offset: Beats
}

impl Deref for SoundBlock {
    type Target = Sound;
    fn deref(&self) -> &Self::Target {&self.sound}
}

impl DerefMut for SoundBlock {
    fn deref_mut(&mut self) -> &mut Self::Target {&mut self.sound}
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
    const Y_BOUND: Range<R64> = r64![0] .. R64::INFINITY;
    const SCALE_Y_BOUND: Range<R64> = r64![5] .. r64![30];
    const OFFSET_Y_BOUND: Range<R64> = r64![-1] .. R64::INFINITY;
    const Y_SNAP: R64 = r64![1];
    type Inner = Sound;
    type Y = i32;
    type VisualContext = ();

    fn create(_: &GraphEditor<Self>, [offset, y]: [R64; 2]) -> Self {
        Self{sound: default(), layer: y.into(), offset}
    }

    fn inner(&self) -> &Self::Inner {&self.sound}
    fn inner_mut(&mut self) -> &mut Self::Inner {&mut self.sound}

    fn y(&self) -> &Self::Y {&self.layer}
    fn y_mut(&mut self) -> &mut Self::Y {&mut self.layer}

    fn loc(&self) -> [R64; 2] {[self.offset, self.layer.into()]}

    fn move_point(point: Result<&mut Self, &mut [R64; 2]>, delta: [R64; 2], _: bool) {
         match point {
            Ok(SoundBlock{layer, offset, ..}) => {
                *offset = r64![0].max(*offset + delta[0]);
                *layer += i32::from(delta[1]);
            }
            Err(point) => {
                point[0] = r64![0].max(point[0] + delta[0]);
                point[1] += delta[1];
            }
        }
    }

    fn in_hitbox(
        &self,
        area:      &[RangeInclusive<R64>; 2],
        _:         &AppContext,
        sequencer: &Sequencer,
        _:         Self::VisualContext
    ) -> AppResult<bool> {
        Ok(area[1].clone().map_bounds(i32::from).contains(&self.layer)
            && (self.offset ..= self.offset + self.sound.len(sequencer.bps())?.max(r64![0.1]))
                .overlap(&area[0]))
    }

    fn fmt_loc(loc: [R64; 2]) -> String {
        format!("{:.3}, layer {}", loc[0], loc[1].floor())
    }

    fn on_selection_change(
        editor: &mut GraphEditor<Self>,
        ctx:    &mut AppContext
    ) -> AppResult<()> {
        Ok(ctx.emit_event(AppEvent::Select(editor.selection().is_empty().not().then_some(0))))
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

        Ok(if let PlaybackContext::All(start) = sequencer.playback_ctx() && start.is_finite() {
            editor.force_redraw();
            let x = (ctx.frame() - start).secs_to_beats(bps) * step[0] - offset[0];
            solid.move_to(*x, 0.0);
            solid.line_to(*x, *canvas_size[1]);
        })
    }

    fn canvas_coords(canvas: &HtmlCanvasElement) -> AppResult<[u32; 2]> {
        let doc = document();
        let w = doc.body().to_app_result()?.client_width()
            - canvas.previous_element_sibling().to_app_result()?
            .client_width();
        let h = canvas.client_height();
        Ok([w as u32, h as u32])
    }
}

impl Display for SoundBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} @ {}", self.sound.name(), Self::fmt_loc(self.loc()))
    }
}

impl SoundBlock {
    pub fn tabs(&self, ctx: &AppContext) -> Html {
        let desc = &AttrValue::from(self.to_string() + ": Settings");
        match self.sound {
            Sound::None => html!{
                <Tab name="Choose Sound Type" {desc} selected=true/>
            },
            Sound::Note{..} | Sound::Noise{..} | Sound::Custom{..} => {
                let setter = ctx.event_emitter().reform(AppEvent::SetTab);
                let id = ctx.selected_tab();
                html!{<>
                    <Tab name="General"  {desc} setter={setter.reform(|_| 0)} selected={id == 0}/>
                    <Tab name="Envelope" {desc} setter={setter.reform(|_| 1)} selected={id == 1}/>
                    <Tab name="Pattern"  {desc} setter={setter.reform(|_| 2)} selected={id == 2}/>
                </>}
            }
        }
    }

}

#[derive(Debug, Clone)]
pub enum PlaybackContext {
    None,
    One(Shared<AudioInput>, Secs),
    All(Secs)
}

impl PlaybackContext {
    pub fn playing(&self)     -> bool {!matches!(self, Self::None)}
    pub fn all_playing(&self) -> bool { matches!(self, Self::All(..))}
    pub fn played_input(&self) -> Option<&Shared<AudioInput>> {
        if let Self::One(x, _) = self {Some(x)} else {None}
    }
}

pub struct Sequencer {
    pattern: Shared<GraphEditor<SoundBlock>>,
    inputs: Vec<Shared<AudioInput>>,
    audio_ctx: BaseAudioContext,
    analyser: AnalyserNode,
    gain: GainNode,
    ctx_created_at: Secs,
    bps: Beats,
    playback_ctx: PlaybackContext
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
            inputs: vec![],
            analyser: audio_ctx.create_analyser()?,
            gain,
            audio_ctx: audio_ctx.into(),
            ctx_created_at: now().to_app_result()? / 1000,
            bps: r64![2],
            playback_ctx: PlaybackContext::None})
    }

    pub fn bps(&self) -> Beats {self.bps}
    pub fn pattern(&self) -> &Shared<GraphEditor<SoundBlock>> {&self.pattern}
    pub fn audio_ctx(&self) -> &BaseAudioContext {&self.audio_ctx}
    pub fn analyser(&self) -> &AnalyserNode {&self.analyser}
    pub fn playback_ctx(&self) -> &PlaybackContext {&self.playback_ctx}

    pub fn volume(&self) -> R32 {
        unsafe{R32::new_unchecked(self.gain.gain().value())}
    }

    pub fn inputs(&self) -> &[Shared<AudioInput>] {&self.inputs}

    pub fn tabs(&self, ctx: &AppContext) -> Html {
        let id = ctx.selected_tab();
        let setter = ctx.event_emitter();
        html!{<>
            <Tab name="General" desc="General settings"
            setter={setter.reform(|_| AppEvent::SetTab(0))}
            selected={id == 0}/>
            <Tab name="Inputs" desc="General settings"
            setter={setter.reform(|_| AppEvent::SetTab(1))}
            selected={id == 1}/>
        </>}
    }

    pub fn params(&self, ctx: &AppContext) -> Html {
        let emitter = ctx.event_emitter();
        match ctx.selected_tab() {
            0 /* General */ => html!{
                <div id="inputs">
                    <Slider key="tmp" name="Tempo"
                    setter={emitter.reform(AppEvent::Bpm)}
                    min={r64![30]} max={r64![240]}
                    postfix="BPM"
                    initial={self.bps * 60}/>
                    <Slider key="gain" name="Master volume"
                    setter={emitter.reform(|x| AppEvent::MasterVolume(R32::from(x)))}
                    initial={self.volume()}/>
                </div>
            },

            1 /* Inputs */ => html!{
                <div class="horizontal-menu dark-bg">
                    {for self.inputs.iter().map(|input| html!{
                        <AudioInputButton
                        playing={self.playback_ctx.played_input().is_some_and(|i| i.eq(input))} 
                        name={input.get().map_or_else(|_| "".into(), |x| x.name().clone())}
                        {input} {emitter} bps={self.bps} class="extend-inner-button-panel"/>
                    })}
                    <Button name="Add audio input" setter={emitter.reform(|_| AppEvent::StartInputAdd)}>
                        <img::Plus/>
                    </Button>
                </div>
            },

            tab_id => html!{
                <p style="color:red">{format!("Invalid tab ID: {tab_id}")}</p>
            }
        }
    }

    pub fn handle_event(self: &mut SharedAwareRefMut<'_, Self>, event: &AppEvent, ctx: &mut AppContext)
    -> AppResult<()> {
        Ok(match *event {
            AppEvent::PreparePlay(ref input) => {
                if self.audio_ctx.is_instance_of::<AudioContext>() {
                    self.playback_ctx = PlaybackContext::None;
                    self.gain.disconnect()?;
                } else {
                    self.audio_ctx = AudioContext::new()?.into();
                    self.analyser = self.audio_ctx.create_analyser()?;
                    self.analyser.connect_with_audio_node(&self.audio_ctx.destination())?;
                    self.ctx_created_at = now().to_app_result()?;
                }
                if let Some(input) = input {
                    input.get_mut()?.bake(self.bps)?;
                } else {
                    for mut block in self.pattern.get_mut()?.iter_mut() {
                        block.inner().prepare(self.bps)?;
                    }
                }
                let volume = self.volume();
                self.gain = self.audio_ctx.create_gain()?;
                self.gain.gain().set_value(*volume);
                self.gain.connect_with_audio_node(&self.analyser)?;
                ctx.emit_event(AppEvent::StartPlay(input.clone()))
            }

            AppEvent::StartPlay(ref input) => {
                let now = now().to_app_result()? - self.ctx_created_at;
                if let Some(input) = input {
                    let player = self.audio_ctx.create_buffer_source()?;
                    {
                        let input = input.get()?;
                        player.set_buffer(input.baked().to_app_result()?.into());
                        player.connect_with_audio_node(&self.gain)?;
                    }
                    self.playback_ctx = PlaybackContext::One(input.clone(), now + self.ctx_created_at);
                    let emitter = ctx.event_emitter().clone();
                    player.set_onended(Some(&js_function!{|| emitter.emit(AppEvent::StopPlay)}));
                    player.start()?;
                } else {
                    self.playback_ctx = PlaybackContext::All(now + self.ctx_created_at);
                    let mut pattern = self.pattern.get_mut()?;
                    for mut block in pattern.iter_mut() {
                        let offset = block.offset.to_secs(self.bps);
                        block.inner().play(&self.gain, now, offset, self.bps)?;
                    }
                }
            }

            AppEvent::StopPlay => {
                self.playback_ctx = PlaybackContext::None;
                self.gain.disconnect()?;
            }

            AppEvent::StartInputAdd => {
                let temp = document().create_element("input")?
                    .unchecked_into::<HtmlInputElement>();
                temp.set_type("file");
                let emitter = ctx.event_emitter().clone();
                temp.set_onchange(Some(&js_function!(|e| emitter.emit(AppEvent::AudioUploaded(e)))));
                temp.click();
            }

            AppEvent::SetInputName(ref e) => {
                let target: HtmlInputElement = e.target_dyn_into().to_app_result()?;
                let to: Rc<str> = target.value().into();
                if !to.is_empty() {
                    let from = ctx.popup().and_then(|x| x.edited_input()).to_app_result()?
                        .get_mut()?.set_name(to.clone());
                    ctx.register_action(AppAction::SetInputName{from, to});
                }
            }

            AppEvent::ReverseInput => {
                ctx.register_action(AppAction::ReverseInput);
                ctx.popup().and_then(Popup::edited_input).to_app_result()?.get_mut()?
                    .changes_mut().reversed.flip();
            }

            AppEvent::SetStartCutOff(to) => {
                let input = ctx.popup().and_then(Popup::edited_input).to_app_result()?;
                let from = replace(&mut input.get_mut()?.changes_mut().cut_start, to);
                ctx.register_action(AppAction::SetStartCutOff{from, to});
            }

            AppEvent::SetEndCutOff(to) => {
                let input = ctx.popup().and_then(Popup::edited_input).to_app_result()?;
                let from = replace(&mut input.get_mut()?.changes_mut().cut_end, to);
                ctx.register_action(AppAction::SetEndCutOff{from, to});
            }

            AppEvent::AudioUploaded(ref e) => {
                let target: HtmlInputElement = e.target_dyn_into().to_app_result()?;
                let emitter = ctx.event_emitter().clone();

                let seq = self.outer();
                spawn_local(async move {
                    let res: AppResult<!> = try {
                        let file = target.files().and_then(|x| x.get(0)).to_app_result()?;
                        let input = AudioInput::new_file(file, seq).await?;
                        return emitter.emit(AppEvent::AddInput(input.into()))
                    };
                    res.report();
                })
            }

            AppEvent::AddInput(ref input) => {
                ctx.register_action(AppAction::AddInput(input.clone()));
                self.inputs.push(input.clone());
            }

            AppEvent::MasterVolume(to) => unsafe {
                let gain = self.gain.gain();
                ctx.register_action(AppAction::SetMasterVolume{from: R32::new_unchecked(gain.value()), to});
                gain.set_value(*to);
            }

            AppEvent::Bpm(mut to) => {
                to /= 60;
                ctx.register_action(AppAction::SetTempo{from: self.bps, to}); 
                self.bps = to
            }

            AppEvent::RedrawEditorPlane => self.pattern.get_mut()?
                .force_redraw(),

            AppEvent::Undo(ref actions) => {
                let mut pat = self.get_sub_ref_mut(|x| &x.pattern)?;
                for action in actions.iter() {
                    match *action {
                        AppAction::SetTempo{from, ..} =>
                            self.bps = from,

                        AppAction::SetMasterVolume{from, ..} =>
                            self.gain.gain().set_value(*from),

                        AppAction::AddInput(_) =>
                            _ = self.inputs.pop(),

                        AppAction::SetInputName{ref from, ..} =>
                            _ = ctx.popup().and_then(|x| x.edited_input())
                                .to_app_result()?.get_mut()?
                                .set_name(from.clone()),

                        AppAction::ReverseInput =>
                            ctx.popup().and_then(|x| x.edited_input())
                                .to_app_result()?.get_mut()?
                                .changes_mut().reversed.flip(),

                        _ => ()
                    }
                }
                pat.handle_event(event, ctx, &*self, || ())?
            }

            AppEvent::Redo(ref actions) => {
                let mut pat = self.get_sub_ref_mut(|x| &x.pattern)?;
                for action in actions.iter() {
                    match *action {
                        AppAction::SetTempo{to, ..} =>
                            self.bps = to,

                        AppAction::SetMasterVolume{to, ..} =>
                            self.gain.gain().set_value(*to),

                        AppAction::AddInput(ref input) =>
                            self.inputs.push(input.clone()),

                        AppAction::SetInputName{ref to, ..} =>
                            _ = ctx.popup().and_then(|x| x.edited_input()).to_app_result()?
                                .get_mut()?.set_name(to.clone()),

                        AppAction::ReverseInput =>
                            ctx.popup().and_then(|x| x.edited_input()).to_app_result()?
                                .get_mut()?.changes_mut().reversed.flip(),

                        _ => ()
                    }
                }
                pat.handle_event(event, ctx, &*self, || ())?
            }

            _ => self.pattern.get_mut()?.handle_event(event, ctx, &*self, || ())?
        })
    }
}
