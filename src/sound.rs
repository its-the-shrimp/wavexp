use std::{
    ops::{Add, Sub, Neg, Range},
    fmt::{self, Display, Formatter, Debug},
    f64::consts::PI,
    mem::{transmute, replace}, cmp::Ordering, rc::Rc};
use js_sys::Math::random;
use wasm_bindgen::{JsValue, JsCast};
use web_sys::{
    AudioNode,
    AudioContext,
    OscillatorNode,
    AudioBufferSourceNode,
    AudioBuffer,
    GainNode,
    Path2d, MouseEvent, Element, DynamicsCompressorNode, AnalyserNode, HtmlCanvasElement, HtmlElement, DragEvent};
use yew::{html, Html, TargetCast, Callback, NodeRef};
use crate::{
    utils::{
        JsResult,
        JsResultUtils,
        R64, R32,
        SaturatingInto, RatioToInt, LooseEq, OptionExt, Pipe, document, HtmlDocumentExt, VecExt},
    input::Slider,
    visual::{GraphEditor, Graphable},
    global::{AppContext, AppEvent},
    loc,
    r32, r64,
};

pub type MSecs = R64;
pub type Secs = R64;
pub type Beats = R64;

pub trait FromBeats {
    fn to_msecs(self, bps: Self) -> MSecs;
    fn to_secs(self, bps: Self) -> Secs;
    fn secs_to_beats(self, bps: Self) -> Beats;
}

impl FromBeats for Beats {
    #[inline]
    fn to_secs(self, bps: Self) -> Secs {self / bps}

    #[inline]
    fn to_msecs(self, bps: Self) -> MSecs {self / bps * r64![1000.0]}

    #[inline]
    fn secs_to_beats(self, bps: Self) -> Beats {self * bps}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, PartialOrd, Ord)]
pub struct Note(u8);

impl Display for Note {
    #[inline] fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", unsafe{Self::NAMES.get_unchecked(self.0 as usize)})
    }
}

impl Add<isize> for Note {
    type Output = Note;
    #[inline] fn add(self, rhs: isize) -> Self::Output {
        Self(self.0.saturating_add_signed(rhs.saturating_into()))
    }
}

impl Sub<isize> for Note {
    type Output = Note;
    #[inline] fn sub(self, rhs: isize) -> Self::Output {
        Self(self.0.saturating_add_signed(rhs.neg().saturating_into()))
    }
}

impl Note {
    pub const MAX: Note = Note(35);
    pub const MIN: Note = Note(0);
    pub const FREQS: [R32; 36] = [
        r32![65.410] /*C2*/, r32![69.300] /*C#2*/,
        r32![73.420] /*D2*/, r32![77.780] /*D#2*/,
        r32![82.410] /*E2*/,
        r32![87.310] /*F2*/, r32![92.500] /*F#2*/,
        r32![98.000] /*G2*/, r32![103.83] /*G#2*/,
        r32![110.00] /*A2*/, r32![116.54] /*A#2*/,
        r32![123.47] /*B2*/,
        r32![130.81] /*C3*/, r32![138.59] /*C#3*/,
        r32![146.83] /*D3*/, r32![155.56] /*D#3*/,
        r32![164.81] /*E3*/,
        r32![174.61] /*F3*/, r32![185.00] /*F#3*/,
        r32![196.00] /*G3*/, r32![207.65] /*G#3*/,
        r32![220.00] /*A3*/, r32![233.08] /*A#3*/,
        r32![246.94] /*B3*/,
        r32![261.63] /*C4*/, r32![277.18] /*C#4*/,
        r32![293.66] /*D4*/, r32![311.13] /*D#4*/,
        r32![329.63] /*E4*/,
        r32![349.23] /*F4*/, r32![369.99] /*F#4*/,
        r32![392.00] /*G4*/, r32![415.30] /*G#4*/,
        r32![440.00] /*A4*/, r32![466.16] /*A#4*/,
        r32![493.88] /*B4*/
    ];

    pub const NAMES: [&'static str; 36] = [
        "C2", "C#2",
        "D2", "D#2",
        "E2",
        "F2", "F#2",
        "G2", "G#2",
        "A2", "A#2",
        "B2",
        "C3", "C#3",
        "D3", "D#3",
        "E3",
        "F3", "F#3",
        "G3", "G#3",
        "A3", "A#3",
        "B3",
        "C4", "C#4",
        "D4", "D#4",
        "E4",
        "F4", "F#4",
        "G4", "G#4",
        "A4", "A#4",
        "B4"];

    #[inline] pub const fn from_index(value: usize) -> Self {
        if value >= Self::FREQS.len() {Self::MAX}
        else {Self(value as u8)}
    }

    #[inline] pub const fn index(&self) -> usize {
        self.0 as usize
    }

    #[inline] pub const fn freq(&self) -> R32 {
        unsafe{*Self::FREQS.get_unchecked(self.0 as usize)}
    }

    #[inline] pub const fn recip(self) -> Self {
        Self(Self::MAX.0 - self.0)
    }
}

pub struct TabInfo {
    pub name: &'static str
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SoundType {
    Note,
    Noise
}

impl SoundType {
    #[inline] pub fn name(&self) -> &'static str {
        match self {
            SoundType::Note => "Note",
            SoundType::Noise => "White Noise"
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PitchPoint {
    pub offset: Beats,
    pub value: Note
}

impl Graphable for PitchPoint {
    const EDITOR_NAME: &'static str = "Pitch Editor";
    const SCALE_X_BOUND: Range<R64> = r64![3.0] .. r64![30.0];
    const SCALE_Y_BOUND: Range<R64> = r64![5.0] .. r64![50.0];
    type Inner = ();
    type Event = ();

    #[inline] fn inner(&self) -> &Self::Inner {&()}
    #[inline] fn inner_mut(&mut self) -> &mut Self::Inner {
        unsafe{transmute(self)}
    }

    #[inline] fn loc(&self) -> [R64; 2] {
        [self.offset, self.value.recip().index().into()]
    }

    fn draw(&self, next: Option<&Self>, mapper: impl Fn([f64; 2]) -> [f64; 2]) -> JsResult<Path2d> {
        let res = Path2d::new().add_loc(loc!())?;
        let src = mapper([*self.offset, self.value.recip().index() as f64]);
        res.ellipse(src[0], src[1], 5.0, 5.0, 0.0, 0.0, PI * 2.0).add_loc(loc!())?;
        if let Some(next) = next {
            let dst = mapper([*next.offset, next.value.recip().index() as f64]);
            res.move_to(src[0], src[1]);
            res.line_to(dst[0], dst[1]);
        }
        Ok(res)
    }

    #[inline] fn set_loc(&mut self, _: usize, self_id: usize, x: impl FnOnce() -> R64, y: impl FnOnce() -> R64)
    -> Option<Self::Event> {
        self.value = Note::from_index(y().to_int()).recip();
        if self_id != 0 {
            let old = replace(&mut self.offset, x());
            (old != self.offset).then_some(())
        } else {None}
    }

    #[inline] fn in_hitbox(&self, point: [R64; 2]) -> bool {
        self.value.recip().index() == *point[1] as usize
            && self.offset.loose_eq(point[0], 0.1)
    }

    #[inline] fn fmt_loc(loc: [R64; 2]) -> String {
        format!("{:.3}, {}", *loc[0], Note::from_index(loc[1].to_int()).recip())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sound {
    Note{gen: OscillatorNode, state: usize, pitch: GraphEditor<PitchPoint>},
    Noise{gen: AudioBufferSourceNode, src: AudioBuffer,
        gain: GainNode, len: Beats, started: bool}
}

impl Sound {
    pub const TYPES: [SoundType; 2] = [
        SoundType::Note,
        SoundType::Noise
    ];

    #[inline] pub fn new(sound_type: SoundType, ctx: &AudioContext) -> JsResult<Self> {
        Ok(match sound_type {
            SoundType::Note => {
                let gen = ctx.create_oscillator().add_loc(loc!())?;
                gen.frequency().set_value(0.0);
                gen.start().add_loc(loc!())?;
                Self::Note{gen, state: 0,
                    pitch: GraphEditor::new(r64![5.0], r64![20.0],
                        vec![PitchPoint{offset: r64![0.0], value: Note::MAX}, PitchPoint{offset: r64![1.0], value: Note::MIN}])}
            }

            SoundType::Noise => {
                let len = ctx.sample_rate();
                let mut src_buf = vec![0.0f32; len as usize];
                src_buf.fill_with(|| random() as f32 * 2.0 - 1.0);
                let src = ctx.create_buffer(2, len as u32, len).add_loc(loc!())?;
                src.copy_to_channel(&src_buf, 0).add_loc(loc!())?;
                src.copy_to_channel(&src_buf, 1).add_loc(loc!())?;
                let gain = ctx.create_gain().add_loc(loc!())?;
                gain.gain().set_value(0.2);
                Self::Noise{gen: JsValue::NULL.unchecked_into(), src, gain,
                    len: r64![1.0], started: false}
            }
        })
    }

    #[inline] pub fn name(&self) -> &'static str {
        match self {
            Sound::Note{..} => "Note",
            Sound::Noise{..} => "Noise"
        }
    }

    pub fn reset(&mut self, ctx: &AudioContext) -> JsResult<()> {
        Ok(match self {
            Sound::Note{state, gen, pitch, ..} => {
                *state = 0;
                let first = unsafe{pitch.get_unchecked(0)};
                gen.frequency().cancel_scheduled_values(0.0).add_loc(loc!())?
                    .set_value(*first.value.freq());
            }

            Sound::Noise{gen, src, started, gain, ..} => {
                *started = false;
                *gen = ctx.create_buffer_source().add_loc(loc!())?;
                gen.set_loop(true);
                gen.set_buffer(Some(src));
                gen.start().add_loc(loc!())?;
                gen.connect_with_audio_node(gain).add_loc(loc!())?;
            }
        })
    }

    pub fn poll(&mut self, plug: &AudioNode, ctx: &AppContext) -> JsResult<Secs> {
        Ok(match self {
            Sound::Note{gen, state, pitch} => {
                if *state == 0 {
                    pitch.force_redraw();
                    gen.connect_with_audio_node(plug).add_loc(loc!())?;
                }
                let cur = unsafe{pitch.data().get_unchecked(*state)};
                *state += 1;
                if let Some(next) = pitch.get(*state) {
                    let res = ctx.now + *(next.offset - cur.offset).to_secs(ctx.bps);
                    gen.frequency()
                        .linear_ramp_to_value_at_time(*cur.value.freq(), *res)
                        .add_loc(loc!())?;
                    res
                } else {
                    pitch.force_redraw();
                    gen.disconnect().add_loc(loc!())?;
                    Secs::INFINITY
                }
            }

            Sound::Noise{gain, len, started, ..} => if *started {
                self.stop(ctx).add_loc(loc!())?;
                Secs::INFINITY
            } else {
                *started = true;
                gain.connect_with_audio_node(plug).add_loc(loc!())?;
                len.to_secs(ctx.bps) + ctx.now
            }
        })
    }

    /// given guarantee: always stop the sound properly regardless of whether an error is returned
    /// expected guarantee: called in the same manner as `poll` would be called
    pub fn stop(&mut self, ctx: &AppContext) -> JsResult<Secs> {
        match self {
            Sound::Note{gen, pitch, state, ..} => match (pitch.data(), pitch.data().len() - *state) {
                ([.., start, end], 2..) => {
                    let res = ctx.now + (end.offset - start.offset).to_secs(ctx.bps);
                    gen.frequency()
                        .linear_ramp_to_value_at_time(*end.value.freq(), *res)
                        .add_loc(loc!()).map(|_| res)
                }

                _ => gen.disconnect().add_loc(loc!()).map(|_| Beats::INFINITY)
            }

            Sound::Noise{gen, ..} => gen.disconnect().add_loc(loc!())
                .map(|_| Beats::INFINITY)
        }
    }

    #[inline] pub fn len(&self) -> Beats {
        match self {
            Sound::Note{pitch, ..} =>
                pitch.data().last().map_or(r64!{0.0}, |x| x.offset),
            Sound::Noise{len, ..} => *len
        }
    }

    #[inline] pub fn tabs(&self) -> &'static [TabInfo] {
        match self {
            Sound::Note{..} =>
                &[],
            Sound::Noise{..} =>
                &[TabInfo{name: "General"}, TabInfo{name: "Volume"}]
        }
    }

    pub fn params(&self, tab_id: usize, setter: Callback<AppEvent>) -> Html {
        match self {
            Sound::Note{pitch, ..} => html!{
                <canvas ref={pitch.canvas().clone()} class="blue-border"
                onpointerdown={setter.reform(AppEvent::FocusTab)}
                onpointerup={setter.reform(|e| AppEvent::HoverTab(MouseEvent::from(e)))}
                onpointermove={setter.reform(|e| AppEvent::HoverTab(MouseEvent::from(e)))}
                onpointerout={setter.reform(|_| AppEvent::LeaveTab)}/>
            },

            Sound::Noise{len, gain, ..} => match tab_id {
                0 /* General */ => html!{<div id="inputs">
                    <Slider key="noise-dur"
                    setter={setter.reform(AppEvent::Duration)}
                    max={r64![100.0]}
                    name="Noise Duration" postfix="Beats"
                    initial={*len}/>
                </div>},
                1 /* Volume */ => html!{<div id="inputs">
                    <Slider key={format!("{self:p}-noise-vol")}
                    setter={setter.reform(|x| AppEvent::Volume(R32::from(x)))}
                    name="Noise Volume"
                    initial={R64::new_or(R64::ZERO, gain.gain().value() as f64)}/>
                </div>},
                tab_id => html!{<p style="color:red">{format!("Invalid tab ID: {tab_id}")}</p>}
            }
        }
    }

    pub fn handle_event(&mut self, event: &AppEvent, ctx: &AppContext) -> JsResult<Option<AppEvent>> {
        Ok(match self {
            Sound::Note{pitch, ..} => match event {
                AppEvent::FocusTab(e) => {
                    e.target_dyn_into::<Element>().to_js_result(loc!())?
                        .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    pitch.handle_hover(Some(e.try_into().add_loc(loc!())?), ctx)
                        .add_loc(loc!())?.map(|_| AppEvent::RedrawEditorPlane)
                }

                AppEvent::HoverTab(e) => pitch
                    .handle_hover(Some(e.try_into().add_loc(loc!())?), ctx)
                    .add_loc(loc!())?.map(|_| AppEvent::RedrawEditorPlane),

                AppEvent::LeaveTab => pitch.handle_hover(None, ctx)
                    .add_loc(loc!())?.map(|_| AppEvent::RedrawEditorPlane),

                AppEvent::Resize => pitch.handle_resize().add_loc(loc!())?
                    .pipe(|_| None),

                AppEvent::Frame(_) => pitch.redraw(ctx).add_loc(loc!())?
                    .map(|[m, a]| AppEvent::SetHint(m, a)),

                AppEvent::LayoutChanged => pitch.init().add_loc(loc!())?
                    .pipe(|_| None),

                _ => None
            }

            Sound::Noise{len, gain, ..} => match event {
                AppEvent::Duration(value) =>
                    *len = *value,
                AppEvent::Volume(value) =>
                    gain.gain().set_value(**value),
                _ => (),
            }.pipe(|_| None)
        })
    }
}

#[derive(Debug)]
pub struct PatternBlock {
    pub sound: Sound,
    pub layer: i32,
    pub offset: Beats
}

impl PartialEq for PatternBlock {
    #[inline] fn eq(&self, other: &Self) -> bool {
        self.offset.eq(&other.offset)
    }
}

impl Eq for PatternBlock {}

impl PartialOrd for PatternBlock {
    #[inline] fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.offset.partial_cmp(&other.offset)
    }
}

impl Ord for PatternBlock {
    #[inline] fn cmp(&self, other: &Self) -> Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl Graphable for PatternBlock {
    const EDITOR_NAME: &'static str = "Editor plane";
    const SCALE_X_BOUND: Range<R64> = r64![5.0] .. r64![95.0];
    const SCALE_Y_BOUND: Range<R64> = r64![5.0] .. r64![30.0];
    type Inner = Sound;
    type Event = AppEvent;
    type Draggable = SoundType;

    #[inline] fn inner(&self) -> &Self::Inner {
        &self.sound
    }

    #[inline] fn inner_mut(&mut self) -> &mut Self::Inner {
        &mut self.sound
    }

    #[inline] fn loc(&self) -> [R64; 2] {
        [self.offset, self.layer.into()]
    }

    #[inline] fn set_loc(&mut self, _: usize, _: usize, x: impl FnOnce() -> R64, y: impl FnOnce() -> R64)
    -> Option<Self::Event> {
        self.offset = x();
        self.layer = y().to_int();
        None
    }

    #[inline] fn desc(&self) -> String {
        self.sound.name().to_owned()
    }

    fn draw(&self, _: Option<&Self>, mapper: impl Fn([f64; 2]) -> [f64; 2]) -> JsResult<Path2d> {
        let res = Path2d::new().add_loc(loc!())?;
        let src = mapper([*self.offset, self.layer as f64]);
        let dst = mapper([*self.offset + *self.sound.len(), (self.layer + 1) as f64]);
        res.rect(src[0], src[1], dst[0] - src[0], dst[1] - src[1]);
        Ok(res)
    }

    #[inline] fn in_hitbox(&self, point: [R64; 2]) -> bool {
        self.layer == *point[1] as i32
            && (self.offset .. self.offset + self.sound.len()).contains(&point[0])
    }

    #[inline] fn fmt_loc(loc: [R64; 2]) -> String {
        format!("{:.3}, layer {}", *loc[0], *loc[1] as i32)
    }

    #[inline] fn on_select(self_id: Option<usize>) -> Option<Self::Event> {
        Some(AppEvent::Select(self_id))
    }

    #[inline] fn on_drop_in(value: Self::Draggable, loc: impl FnOnce() -> [R64; 2]) -> Option<Self::Event> {
        let loc = loc();
        Some(AppEvent::Add(value, loc[1].to_int(), loc[0]))
    }

    #[inline] fn draw_draggable(_draggable: Self::Draggable, step: [R64; 2])
    -> JsResult<Path2d> {
        let res = Path2d::new().add_loc(loc!())?;
        let [[x1, x2], [y1, y2]] = step.map(|x| [x / -2i8, x / 2i8]);
        res.move_to(*x1, *y1);
        res.line_to(*x2, *y1);
        res.move_to(0.0, *y1);
        res.line_to(0.0, *y2);
        res.move_to(*x1, *y2);
        res.line_to(*x2, *y2);
        Ok(res)
    }
}

pub enum SequencerState {
    Start,
    Play{next: usize},
    Idle,
    Stop,
    None
}

pub struct Sequencer {
    pattern: GraphEditor<PatternBlock>,
    pending: Vec<(usize, Secs)>,
    state: SequencerState,
    audio_ctx: AudioContext,
    plug: DynamicsCompressorNode,
    gain: GainNode
}

impl Sequencer {
    #[inline] pub fn new(audio_ctx: AudioContext, visualiser: Rc<AnalyserNode>) -> JsResult<Self> {
        let plug = DynamicsCompressorNode::new(&audio_ctx).add_loc(loc!())?;
        plug.ratio().set_value(20.0);
        plug.release().set_value(1.0);
        let gain = GainNode::new(&audio_ctx).add_loc(loc!())?;
        gain.gain().set_value(0.2);

        plug.connect_with_audio_node(&visualiser).add_loc(loc!())?
            .connect_with_audio_node(&gain).add_loc(loc!())?
            .connect_with_audio_node(&audio_ctx.destination()).add_loc(loc!())?;

        Ok(Self{pattern: GraphEditor::new(r64![20.0], r64![10.0], vec![]), pending: vec![], audio_ctx,
            state: SequencerState::None, plug, gain})
    }

    #[inline] pub fn gain(&self) -> R32 {
        R32::new_or(R32::ZERO, self.gain.gain().value())
    }

    #[inline] pub fn canvas(&self) -> &NodeRef {
        self.pattern.canvas()
    }

    #[inline] pub fn pattern_mut(&mut self) -> &mut GraphEditor<PatternBlock> {
        &mut self.pattern
    }

    #[inline] pub fn pattern(&self) -> &GraphEditor<PatternBlock> {
        &self.pattern
    }

    pub fn handle_event(&mut self, event: &AppEvent, ctx: &AppContext) -> JsResult<Option<AppEvent>> {
        Ok(match event {
            &AppEvent::Add(ty, layer, offset) => self.pattern.add_point(PatternBlock{
                sound: Sound::new(ty, &self.audio_ctx).add_loc(loc!())?,
                layer, offset})
                .pipe(|_| None),

            AppEvent::TogglePlay => match self.state {
                SequencerState::None => for mut block in self.pattern.iter_mut() {
                    block.inner().reset(&self.audio_ctx).add_loc(loc!())?;
                }.pipe(|_| SequencerState::Start),

                SequencerState::Idle => SequencerState::Stop,
                
                _ => SequencerState::Stop,
            }.pipe(|x| {self.state = x; None}),

            AppEvent::Resize => {
                let canvas: HtmlCanvasElement = self.pattern.canvas()
                    .cast().to_js_result(loc!())?;
                let doc = document();
                let w = doc.body().to_js_result(loc!())?.client_width()
                    - doc.element_dyn_into::<HtmlElement>("ctrl-panel").add_loc(loc!())?
                    .client_width();
                canvas.set_width(w as u32);
                let h = canvas.client_height();
                canvas.set_height(h as u32);
                self.pattern.force_redraw();
                None
            }

            AppEvent::RedrawEditorPlane => self.pattern.force_redraw()
                .pipe(|_| None),

            AppEvent::LayoutChanged => self.pattern.init().add_loc(loc!())?
                .pipe(|_| None),

            AppEvent::FocusPlane(e) => {
                e.target_dyn_into::<Element>().to_js_result(loc!())?
                    .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                self.pattern.handle_hover(Some(e.try_into().add_loc(loc!())?), ctx)
                    .add_loc(loc!())?
            }

            AppEvent::HoverPlane(e) => {
                if let Some(e) = e.dyn_ref::<DragEvent>() {
                    e.prevent_default();
                }
                self.pattern.handle_hover(Some(e.try_into().add_loc(loc!())?), ctx)
                    .add_loc(loc!())?
            }

            AppEvent::LeavePlane => self.pattern.handle_hover(None, ctx)
                .add_loc(loc!())?,

            AppEvent::SetBlockAdd(d) => {
                self.pattern.set_draggable(*d);
                None
            }

            AppEvent::MasterGain(value) => self.gain.gain()
                .set_value(**value).pipe(|_| None),

            AppEvent::Frame(_) => {
                match self.state {
                    SequencerState::Start => {
                        let mut next = 0;
                        for mut block in self.pattern.iter_mut().take_while(|x| *x.offset == 0.0) {
                            let when = block.inner().poll(&self.plug, ctx).add_loc(loc!())?;
                            self.pending.push_sorted_by_key((next, when), |x| x.1);
                            next += 1;
                        }
                        self.state = SequencerState::Play{next};
                        self.pattern.force_redraw();
                    }

                    SequencerState::Play{ref mut next} => {
                        let offset = (ctx.now - ctx.play_since).secs_to_beats(ctx.bps);
                        for mut block in self.pattern.iter_mut().skip(*next).take_while(|x| x.offset <= offset) {
                            let when = block.inner().poll(&self.plug, ctx).add_loc(loc!())?;
                            self.pending.push_sorted_by_key((*next, when), |x| x.1);
                            *next += 1;
                        }
                        if *next >= self.pattern.data().len() {
                            self.state = SequencerState::Idle;
                        }
                    }

                    SequencerState::Stop => {
                        let mut err = Ok(());
                        self.pending.drain_filter(|(id, when)| unsafe {
                            *when = self.pattern.get_unchecked_mut(*id)
                                .inner().stop(ctx).add_loc(loc!())
                                .unwrap_or_else(|x| {err = Err(x); R64::INFINITY});
                            when.is_finite()
                        });
                        err?;
                        self.state = SequencerState::None;
                    }

                    SequencerState::Idle | SequencerState::None => ()
                };

                let n_due = self.pending.iter().position(|x| x.1 > ctx.now).unwrap_or(self.pending.len());
                let due: Vec<usize> = self.pending.drain(..n_due).map(|x| x.0).collect::<Vec<_>>();
                for id in due {
                    let mut block = unsafe{self.pattern.get_unchecked_mut(id)};
                    let when = block.inner().poll(&self.plug, ctx).add_loc(loc!())?;
                    if when.is_infinite() {continue}
                    self.pending.push_sorted_by_key((id, when), |x| x.0);
                }

                self.pattern.redraw(ctx).add_loc(loc!())?.map(|[m, a]| AppEvent::SetHint(m, a))
            }

            _ => None
        })
    }
}
