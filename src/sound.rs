use std::{
    ops::{Add, Sub, Neg, Range},
    fmt::{self, Display, Formatter, Debug},
    f64::consts::PI,
    mem::transmute,
    cmp::Ordering,
    rc::Rc, borrow::Cow};
use js_sys::Math::random;
use web_sys::{
    AudioNode,
    AudioContext,
    OscillatorNode,
    AudioBufferSourceNode,
    AudioBuffer,
    GainNode,
    Path2d, MouseEvent, Element, DynamicsCompressorNode, AnalyserNode, HtmlElement};
use yew::{html, Html, TargetCast, Callback, NodeRef};
use crate::{
    utils::{
        JsResult,
        JsResultUtils,
        R64, R32,
        SaturatingInto, LooseEq, OptionExt, Pipe, document, HtmlDocumentExt, VecExt, js_error},
    input::{Slider, Button},
    visual::{GraphEditor, Graphable},
    global::{AppContext, AppEvent},
    loc,
    r32, r64
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

    #[inline] pub fn freq(&self) -> R32 {
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
    // TODO: make this generic over the number of defined notes
    const Y_BOUND: Range<R64> = r64![0.0] .. r64![36.0];
    const SCALE_Y_BOUND: Range<R64> = r64![40.0] .. r64![40.0];
    const OFFSET_Y_BOUND: Range<R64> = r64![-2.0] .. r64![-2.0];
    const Y_SNAP: R64 = r64![1.0];
    type Inner = ();
    type Event = (R64, Note);

    #[inline] fn inner(&self) -> &Self::Inner {&()}
    #[inline] fn inner_mut(&mut self) -> &mut Self::Inner {
        unsafe{transmute(self)}
    }

    #[inline] fn loc(&self) -> [R64; 2] {
        [self.offset, self.value.recip().index().into()]
    }

    #[inline] fn set_loc(&mut self, n_points: usize, self_id: usize, x: impl FnOnce() -> R64, y: impl FnOnce() -> R64)
    -> Option<Self::Event> {
        self.value = Note::from_index(y().into()).recip();
        if ![0, n_points - 2].contains(&self_id) {
            self.offset = x();
        }
        None
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

    fn draw_meta_held(canvas_size: [R64; 2], _: [R64; 2]) -> JsResult<Path2d> {
        let res = Path2d::new().add_loc(loc!())?;
        res.move_to(-*canvas_size[0], 0.0);
        res.line_to( *canvas_size[0], 0.0);
        res.move_to(0.0, -*canvas_size[1]);
        res.line_to(0.0,  *canvas_size[1]);
        Ok(res)
    }

    #[inline] fn on_meta_click(loc: impl FnOnce() -> Option<[R64; 2]>) -> Option<Self::Event> {
        loc().map(|[x, y]| (x, Note::from_index(y.into()).recip()))
    }

    #[inline] fn in_hitbox(&self, point: [R64; 2]) -> bool {
        (self.value.recip().index() as f64).loose_eq(*point[1], 0.5)
            && self.offset.loose_eq(point[0], 0.25)
    }

    #[inline] fn fmt_loc(loc: Option<[R64; 2]>) -> String {
        if let Some([x, y]) = loc {
            format!("{x:.3}, {}", Note::from_index(y.into()).recip())
        } else {"--.---, --".to_owned()}
    }

    #[inline] fn meta_held_hint(loc: Option<[R64; 2]>) -> Option<[Cow<'static, str>; 2]> {
        Some(["Pitch Editor: adding new point".into(), loc.map_or_default(|[x, y]|
            format!("Click to add a point at {x:.3}, {}", Note::from_index(y.into()).recip()).into())])
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VolumePoint {
    offset: R64,
    value: R32
}

impl Graphable for VolumePoint {
    const EDITOR_NAME: &'static str = "Volume Editor";
    const Y_BOUND: Range<R64> = r64![0.0] .. r64![10.0];
    const SCALE_Y_BOUND: Range<R64> = r64![12.0] .. r64![12.0];
    const OFFSET_Y_BOUND: Range<R64> = r64![-1.0] .. r64![-1.0];
    const Y_SNAP: R64 = r64![0.0];
    type Inner = ();
    type Event = (R64, R32);

    #[inline] fn inner(&self) -> &Self::Inner {&()}
    #[inline] fn inner_mut(&mut self) -> &mut Self::Inner {
        unsafe{transmute(self)}
    }

    #[inline] fn loc(&self) -> [R64; 2] {
        [self.offset, (self.value * -10i8 + 10u8).into()]
    }

    #[inline] fn set_loc(&mut self, n_points: usize, self_id: usize, x: impl FnOnce() -> R64, y: impl FnOnce() -> R64)
    -> Option<Self::Event> {
        self.value = (y() / -10i8 + 1u8).into();
        if ![0, n_points - 2].contains(&self_id) {
            self.offset = x();
        }
        None
    }

    #[inline] fn on_meta_click(loc: impl FnOnce() -> Option<[R64; 2]>) -> Option<Self::Event> {
        loc().map(|[x, y]| (x, R32::from(y) / -10i8 + 1u8))
    }

    fn draw(&self, next: Option<&Self>, mapper: impl Fn([f64; 2]) -> [f64; 2]) -> JsResult<Path2d> {
        let res = Path2d::new().add_loc(loc!())?;
        let src = mapper([*self.offset, *self.value as f64 * -10.0 + 10.0]);
        res.ellipse(src[0], src[1], 5.0, 5.0, 0.0, 0.0, PI * 2.0).add_loc(loc!())?;
        if let Some(next) = next {
            let dst = mapper([*next.offset, *next.value as f64 * -10.0 + 10.0]);
            res.move_to(src[0], src[1]);
            res.line_to(dst[0], dst[1]);
        }
        Ok(res)
    }

    fn draw_meta_held(canvas_size: [R64; 2], _: [R64; 2]) -> JsResult<Path2d> {
        let res = Path2d::new().add_loc(loc!())?;
        res.move_to(-*canvas_size[0], 0.0);
        res.line_to( *canvas_size[0], 0.0);
        res.move_to(0.0, -*canvas_size[1]);
        res.line_to(0.0,  *canvas_size[1]);
        Ok(res)
    }

    #[inline] fn in_hitbox(&self, point: [R64; 2]) -> bool {
        (*self.value * -10.0 + 10.0).loose_eq(*point[1] as f32, 0.25)
            && self.offset.loose_eq(point[0], 0.25)
    }

    #[inline] fn fmt_loc(loc: Option<[R64; 2]>) -> String {
        if let Some([x, y]) = loc {
            format!("{x:.3}, {:.3}%", *y * 10.0)
        } else {"--.--- --%".to_owned()}
    }

    #[inline] fn meta_held_hint(loc: Option<[R64; 2]>) -> Option<[Cow<'static, str>; 2]> {
        Some(["Volume Editor: adding new point".into(), loc.map_or_default(|[x, y]|
            format!("Click to add a point at {x:.3}, {:.3}%", *y * 10.0).into())])
    }
}

#[derive(Default, Debug, Clone)]
pub enum Sound {
    #[default] None,
    Note{gen: OscillatorNode, gain: GainNode,
        pitch: GraphEditor<PitchPoint>, volume: GraphEditor<VolumePoint>},
    Noise{gen: AudioBufferSourceNode, src: AudioBuffer,
        gain: GainNode, len: Beats}
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
                let gain = ctx.create_gain().add_loc(loc!())?;
                gain.gain().set_value(1.0);
                gen.frequency().set_value(0.0);
                gen.start().add_loc(loc!())?;
                gen.connect_with_audio_node(&gain).add_loc(loc!())?;
                Self::Note{gen, gain,
                    pitch: GraphEditor::new(vec![
                        PitchPoint{offset: r64![0.0], value: Note::MAX},
                        PitchPoint{offset: r64![1.0], value: Note::MAX},
                        PitchPoint{offset: r64![1.5], value: Note::MIN}]),
                    volume: GraphEditor::new(vec![
                        VolumePoint{offset: r64![0.0], value: r32![1.0]},
                        VolumePoint{offset: r64![1.0], value: r32![1.0]},
                        VolumePoint{offset: r64![1.5], value: r32![0.0]}])}
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
                Self::Noise{gen: ctx.create_buffer_source().add_loc(loc!())?,
                    src, gain, len: r64![1.0]}
            }
        })
    }

    #[inline] pub fn name(&self) -> &'static str {
        match self {
            Sound::None => "Undefined",
            Sound::Note{..} => "Note",
            Sound::Noise{..} => "Noise"
        }
    }

    /// called before starting to play the sounds to reset their state and allow them to schedule
    /// their starting events
    pub fn reset(&mut self, ctx: &AppContext, self_id: usize, self_offset: Beats, mut scheduler: impl FnMut(SoundEvent))
    -> JsResult<()> {
        Ok(match self {
            Sound::None => (),

            Sound::Note{gen, gain, pitch, volume, ..} => {
                gen.frequency().cancel_scheduled_values(0.0).add_loc(loc!())?
                    .set_value(*unsafe{pitch.get_unchecked(0)}.value.freq());
                scheduler(SoundEvent::Pitch{id: self_id, when: self_offset, state: 0});

                gain.gain().cancel_scheduled_values(0.0).add_loc(loc!())?
                    .set_value(*unsafe{volume.get_unchecked(0)}.value);
                scheduler(SoundEvent::Volume{id: self_id, when: self_offset, state: 0});
            }

            Sound::Noise{gen, src, gain, ..} => {
                gen.disconnect().add_loc(loc!())?;
                *gen = ctx.audio_ctx.create_buffer_source().add_loc(loc!())?;
                gen.set_loop(true);
                gen.set_buffer(Some(src));
                gen.start().add_loc(loc!())?;
                gen.connect_with_audio_node(gain).add_loc(loc!())?;
                scheduler(SoundEvent::Start{id: self_id, when: self_offset})
            }
        })
    }

    pub fn poll(&mut self, plug: &AudioNode, ctx: &SoundContext, src: SoundEvent, mut scheduler: impl FnMut(SoundEvent)) -> JsResult<()> {
        Ok(match self {
            Sound::None => (),

            Sound::Note{gen, gain, pitch, volume} => match src {
                SoundEvent::Pitch{id, when, mut state} => {
                    if state == 0 {pitch.force_redraw()}
                    state += 1;
                    if let Some(PitchPoint{offset, value}) = pitch.data().get(state) {
                        let when = when + offset;
                        gen.frequency().linear_ramp_to_value_at_time(*value.freq(),
                            *when.to_secs(ctx.bps) + *ctx.play_since).add_loc(loc!())?;
                        scheduler(SoundEvent::Pitch{id, when, state})
                    }
                }

                SoundEvent::Volume{id, when, mut state} => {
                    if state == 0 {
                        volume.force_redraw();
                        gain.connect_with_audio_node(plug).add_loc(loc!())?;
                    }
                    state += 1;
                    if let Some(VolumePoint{offset, value}) = volume.data().get(state) {
                        let when = when + offset;
                        gain.gain().linear_ramp_to_value_at_time(**value,
                            *when.to_secs(ctx.bps) + *ctx.play_since).add_loc(loc!())?;
                        scheduler(SoundEvent::Volume{id, when, state})
                    }
                }

                SoundEvent::Stop{..} => gain.disconnect().add_loc(loc!())?,

                src => js_error(format!("invalid event: {src:?}"), loc!())?,
            }

            Sound::Noise{gen, gain, len, ..} => match src {
                SoundEvent::Start{id, ..} => {
                    gain.connect_with_audio_node(plug).add_loc(loc!())?;
                    scheduler(SoundEvent::Stop{id, when: *len});
                }

                SoundEvent::Stop{..} => {
                    gain.disconnect().add_loc(loc!())?;
                    gen.disconnect().add_loc(loc!())?;
                }

                src => js_error(format!("invalid event: {src:?}"), loc!())?,
            }
        })
    }

    #[inline] pub fn len(&self) -> Beats {
        match self {
            Sound::None => r64![1.0],
            Sound::Note{pitch, ..} =>
                pitch.data().get(pitch.data().len() - 2)
                    .map_or(r64!{0.0}, |x| x.offset),
            Sound::Noise{len, ..} => *len
        }
    }

    #[inline] pub fn tabs(&self) -> &'static [TabInfo] {
        match self {
            Sound::None =>
                &[TabInfo{name: "Choose Sound Type"}],
            Sound::Note{..} =>
                &[TabInfo{name: "General"}, TabInfo{name: "Volume"}, TabInfo{name: "Pitch"}],
            Sound::Noise{..} =>
                &[TabInfo{name: "General"}, TabInfo{name: "Volume"}]
        }
    }

    pub fn params(&self, tab_id: usize, setter: Callback<AppEvent>) -> Html {
        match self {
            Sound::None => html!{<div id="block-add-menu">
                {for Sound::TYPES.iter().map(|x| html!{
                    <Button name={x.name()}
                        setter={setter.reform(|_| AppEvent::SetBlockType(*x))}>
                        <p>{x.name()}</p>
                    </Button>
                })}
            </div>},

            Sound::Note{pitch, volume, ..} => match tab_id {
                0 /* General */ => html!{<div id="inputs">
                    <Slider key="note-dur"
                    setter={setter.reform(AppEvent::Duration)}
                    max={r64![100.0]}
                    name="Note Duration" postfix="Beats"
                    initial={self.len()}/>
                </div>},
                1 /* Volume */ => html!{
                    <canvas ref={volume.canvas().clone()} class="blue-border"
                    onpointerdown={setter.reform(AppEvent::FocusTab)}
                    onpointerup={setter.reform(|e| AppEvent::HoverTab(MouseEvent::from(e)))}
                    onpointermove={setter.reform(|e| AppEvent::HoverTab(MouseEvent::from(e)))}
                    onpointerout={setter.reform(|_| AppEvent::LeaveTab)}
                    ondblclick={setter.reform(AppEvent::DoubleClickTab)}/>
                },
                2 /* Pitch */ => html!{
                    <canvas ref={pitch.canvas().clone()} class="blue-border"
                    onpointerdown={setter.reform(AppEvent::FocusTab)}
                    onpointerup={setter.reform(|e| AppEvent::HoverTab(MouseEvent::from(e)))}
                    onpointermove={setter.reform(|e| AppEvent::HoverTab(MouseEvent::from(e)))}
                    onpointerout={setter.reform(|_| AppEvent::LeaveTab)}
                    ondblclick={setter.reform(AppEvent::DoubleClickTab)}/>
                },
                tab_id => html!{<p style="color:red">{format!("Invalid tab ID: {tab_id}")}</p>}
            }

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
            Sound::None => if let AppEvent::SetBlockType(ty) = event {
                *self = Self::new(*ty, &ctx.audio_ctx).add_loc(loc!())?;
            }.pipe(|_| None),

            Sound::Note{pitch, volume, ..} => match event {
                AppEvent::FocusTab(e) => {
                    e.target_dyn_into::<Element>().to_js_result(loc!())?
                        .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    if ctx.selected_tab == 1 {
                        volume.handle_hover(Some(e.try_into().add_loc(loc!())?), ctx)
                            .add_loc(loc!())?;
                    } else if ctx.selected_tab == 2 {
                        pitch.handle_hover(Some(e.try_into().add_loc(loc!())?), ctx)
                            .add_loc(loc!())?;
                    }
                    None
                }

                AppEvent::HoverTab(e) if ctx.selected_tab == 1 =>
                    match volume.handle_hover(Some(e.try_into().add_loc(loc!())?), ctx).add_loc(loc!())? {
                        Some((offset, value)) =>
                            volume.add_point(VolumePoint{offset, value}).pipe(|_| None),
                        None => None
                    }

                AppEvent::HoverTab(e) if ctx.selected_tab == 2 =>
                    match pitch.handle_hover(Some(e.try_into().add_loc(loc!())?), ctx).add_loc(loc!())? {
                        Some((offset, value)) =>
                            pitch.add_point(PitchPoint{offset, value}).pipe(|_| None),
                        None => None
                    }

                AppEvent::LeaveTab if ctx.selected_tab == 1 => volume
                    .handle_hover(None, ctx).add_loc(loc!())?
                    .pipe(|_| None),

                AppEvent::LeaveTab if ctx.selected_tab == 2 => pitch
                    .handle_hover(None, ctx).add_loc(loc!())?
                    .pipe(|_| None),

                AppEvent::AfterSetTab(1) => volume
                    .init(|c| Ok([c.client_width() as u32, c.client_height() as u32]))
                    .add_loc(loc!())?.pipe(|_| None),

                AppEvent::AfterSetTab(2) => pitch
                    .init(|c| Ok([c.client_width() as u32, c.client_height() as u32]))
                    .add_loc(loc!())?.pipe(|_| None),

                AppEvent::Frame(_) if ctx.selected_tab == 1 => volume
                    .redraw(ctx).add_loc(loc!())?
                    .map(|[m, a]| AppEvent::SetHint(m, a)),

                AppEvent::Frame(_) if ctx.selected_tab == 2 => pitch
                    .redraw(ctx).add_loc(loc!())?
                    .map(|[m, a]| AppEvent::SetHint(m, a)),

                AppEvent::Duration(len) => unsafe {
                    let last_id = pitch.len() - 1;
                    let old_len = pitch.get_unchecked(last_id - 1).offset;
                    for point in pitch.iter_mut().take(last_id) {
                        point.unlock().offset *= len / old_len;
                    }
                    pitch.get_unchecked_mut(last_id).unlock().offset += len - old_len;

                    let last_id = volume.len() - 1;
                    let old_len = volume.get_unchecked(last_id - 1).offset;
                    for point in volume.iter_mut().take(last_id) {
                        point.unlock().offset *= len / old_len;
                    }
                    volume.get_unchecked_mut(last_id).unlock().offset += len - old_len;

                    Some(AppEvent::RedrawEditorPlane)
                }

                _ => None
            }

            Sound::Noise{len, gain, ..} => match event {
                AppEvent::Duration(value) => {
                    *len = *value;
                    Some(AppEvent::RedrawEditorPlane)
                }

                AppEvent::Volume(value) => {
                    gain.gain().set_value(**value);
                    None
                }

                _ => None,
            }
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SoundContext {
    pub bps: Beats,
    pub play_since: Secs,
    pub now: Secs
}

impl Default for SoundContext {
    #[inline] fn default() -> Self {
        Self{bps: r64![2.0], play_since: Secs::NEG_INFINITY, now: r64![0.0]}
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
    const Y_BOUND: Range<R64> = r64![0.0] .. R64::INFINITY;
    const SCALE_Y_BOUND: Range<R64> = r64![5.0] .. r64![30.0];
    const OFFSET_Y_BOUND: Range<R64> = r64![-1.0] .. R64::INFINITY;
    const Y_SNAP: R64 = r64![1.0];
    type Inner = Sound;
    type Event = AppEvent;

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
        self.layer = y().into();
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

    fn draw_meta_held(canvas_size: [R64; 2], _: [R64; 2]) -> JsResult<Path2d> {
        let res = Path2d::new().add_loc(loc!())?;
        res.move_to(-*canvas_size[0], 0.0);
        res.line_to( *canvas_size[0], 0.0);
        res.move_to(0.0, -*canvas_size[1]);
        res.line_to(0.0,  *canvas_size[1]);
        Ok(res)
    }

    #[inline] fn in_hitbox(&self, point: [R64; 2]) -> bool {
        self.layer == *point[1] as i32
            && (self.offset .. self.offset + self.sound.len()).contains(&point[0])
    }

    #[inline] fn fmt_loc(loc: Option<[R64; 2]>) -> String {
        if let Some([x, y]) = loc {
            format!("{x:.3}, layer {}", *y as i32)
        } else {"--.---, layer --".to_owned()}
    }

    #[inline] fn on_select(self_id: Option<usize>) -> Option<Self::Event> {
        Some(AppEvent::Select(self_id))
    }

    #[inline] fn on_meta_click(loc: impl FnOnce() -> Option<[R64; 2]>) -> Option<Self::Event> {
        loc().map(|[x, y]| AppEvent::Add(y.into(), x))
    }

    #[inline] fn meta_held_hint(loc: Option<[R64; 2]>) -> Option<[Cow<'static, str>; 2]> {
        Some(["Editor plane: adding new block".into(), loc.map_or_default(|[x, y]|
            format!("Click to add a block at {x:.3}, layer {y:.0}").into())])
    }
}

/// all the `when` fields are offsets from the start in beats
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SoundEvent {
    Pitch {id: usize, when: Beats, state: usize},
    Volume{id: usize, when: Beats, state: usize},
    Start {id: usize, when: Beats},
    Stop  {id: usize, when: Beats}
}

impl PartialOrd for SoundEvent {
    #[inline] fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.when().partial_cmp(&other.when())
    }
}

impl Ord for SoundEvent {
    #[inline] fn cmp(&self, other: &Self) -> Ordering {
        self.when().cmp(&other.when())
    }
}

impl SoundEvent {
    #[inline] fn target(&self) -> usize {
        match self {
            SoundEvent::Start{id, ..}
            | SoundEvent::Stop{id, ..}
            | SoundEvent::Pitch{id, ..}
            | SoundEvent::Volume{id, ..} => *id
        }
    }

    #[inline] fn when(&self) -> Secs {
        match self {
            SoundEvent::Start{when, ..}
            | SoundEvent::Stop{when, ..}
            | SoundEvent::Pitch{when, ..} 
            | SoundEvent::Volume{when, ..} => *when,
        }
    }
}

pub struct Sequencer {
    pattern: GraphEditor<PatternBlock>,
    pending: Vec<SoundEvent>,
    plug: DynamicsCompressorNode,
    gain: GainNode,
    playing: bool,
    used_to_play: bool
}

impl Sequencer {
    #[inline] pub fn new(audio_ctx: &AudioContext, visualiser: Rc<AnalyserNode>) -> JsResult<Self> {
        let plug = DynamicsCompressorNode::new(audio_ctx).add_loc(loc!())?;
        plug.ratio().set_value(20.0);
        plug.release().set_value(1.0);
        let gain = GainNode::new(audio_ctx).add_loc(loc!())?;
        gain.gain().set_value(0.2);

        plug.connect_with_audio_node(&visualiser).add_loc(loc!())?
            .connect_with_audio_node(&gain).add_loc(loc!())?
            .connect_with_audio_node(&audio_ctx.destination()).add_loc(loc!())?;

        Ok(Self{plug, gain, pattern: GraphEditor::new(vec![]), pending: vec![],
            playing: false, used_to_play: false})
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
            &AppEvent::Add(layer, offset) => self.pattern
                .add_point(PatternBlock{sound: Sound::default(), layer, offset})
                .pipe(|_| None),

            AppEvent::Select(mut id) => {
                id = id.filter(|x| Some(*x) != self.pattern.fixed_id());
                self.pattern.fix_point(id).add_loc(loc!())?;
                None
            }

            AppEvent::Remove => self.pattern
                .del_fixed().to_js_result(loc!())?.pipe(|_| None),

            AppEvent::TogglePlay => if self.playing {
                self.pending.clear();
                self.plug.disconnect().add_loc(loc!())?;
                self.plug = ctx.audio_ctx.create_dynamics_compressor().add_loc(loc!())?;
                self.plug.ratio().set_value(20.0);
                self.plug.release().set_value(1.0);
                self.plug.connect_with_audio_node(&self.gain).add_loc(loc!())?;
                self.playing = false;
                self.used_to_play = false;
            } else {
                self.pending.clear();
                for (id, mut block) in self.pattern.iter_mut().enumerate() {
                    let offset = block.offset;
                    block.inner().reset(ctx, id, offset,
                        |x| _ = self.pending.push_sorted(x)).add_loc(loc!())?;
                }
                self.playing = true;
            }.pipe(|_| None),

            AppEvent::Resize => self.pattern.init(|canvas| {
                let doc = document();
                let w = doc.body().to_js_result(loc!())?.client_width()
                    - doc.element_dyn_into::<HtmlElement>("ctrl-panel").add_loc(loc!())?
                    .client_width();
                let h = canvas.client_height();
                Ok([w as u32, h as u32])
            }).add_loc(loc!())?.pipe(|_| None),

            AppEvent::RedrawEditorPlane => self.pattern.force_redraw()
                .pipe(|_| None),

            AppEvent::FocusPlane(e) => {
                e.target_dyn_into::<Element>().to_js_result(loc!())?
                    .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                self.pattern.handle_hover(Some(e.try_into().add_loc(loc!())?), ctx)
                    .add_loc(loc!())?
            }

            AppEvent::HoverPlane(e) =>
                self.pattern.handle_hover(Some(e.try_into().add_loc(loc!())?), ctx)
                    .add_loc(loc!())?,

            AppEvent::LeavePlane => self.pattern.handle_hover(None, ctx)
                .add_loc(loc!())?,

            AppEvent::MasterGain(value) => self.gain.gain()
                .set_value(**value).pipe(|_| None),

            AppEvent::Frame(_) => {
                let to_emit = if self.playing {
                    let (to_emit, now) = if self.used_to_play {
                        (None, (ctx.now - ctx.play_since).secs_to_beats(ctx.bps))
                    } else {
                        self.used_to_play = true;
                        self.pattern.force_redraw();
                        (Some(AppEvent::AudioStarted(ctx.now)), r64![0.0])
                    };
                    let n_due = self.pending.iter().position(|x| x.when() > now).unwrap_or(self.pending.len());
                    for event in self.pending.drain(..n_due).collect::<Vec<_>>() {
                        let id = event.target();
                        let mut block = unsafe{self.pattern.get_unchecked_mut(id)};

                        let mut stop = !matches!(event, SoundEvent::Stop{..});
                        block.inner().poll(&self.plug, ctx, event, |new| {
                            self.pending.push_sorted(new);
                            stop = false;
                        }).add_loc(loc!())?;

                        if stop {
                            block.inner() .poll(&self.plug, ctx, SoundEvent::Stop{id, when: now}, |_| ())
                                .add_loc(loc!())?;
                        }
                    }
                    to_emit
                } else {None};

                if let x @Some(_) = to_emit {x} else {
                    self.pattern.redraw(ctx).add_loc(loc!())?.map(|[m, a]| AppEvent::SetHint(m, a))
                }
            }

            _ => None
        })
    }
}
