use std::{
    ops::{Add, Sub, Range},
    fmt::{self, Display, Formatter, Debug},
    mem::replace,
    cmp::Ordering,
    rc::Rc,
    borrow::Cow};
use js_sys::Math::random;
use web_sys::{
    AudioNode,
    AudioContext,
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
        LooseEq, OptionExt, Pipe, document, HtmlDocumentExt, VecExt, js_error, Take, RangeExt, SliceRef},
    input::{Slider, Button},
    visual::{GraphEditor, Graphable},
    global::{AppContext, AppEvent},
    loc,
    r32,
    r64
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
        Display::fmt(unsafe{Self::NAMES.get_unchecked(self.0 as usize)}, f)
    }
}

impl Add<isize> for Note {
    type Output = Note;
    #[inline] fn add(self, rhs: isize) -> Self::Output {
        Self((0 .. Self::N_NOTES as isize).fit((self.0 as isize).saturating_add(rhs)) as u8)
    }
}

impl Sub<isize> for Note {
    type Output = Note;
    #[inline] fn sub(self, rhs: isize) -> Self::Output {
        Self((0 .. Self::N_NOTES as isize).fit((self.0 as isize).saturating_sub(rhs)) as u8)
    }
}

impl Note {
    pub const MAX: Note = Note(35);
    // pub const MIN: Note = Note(0);
    pub const N_NOTES: usize = Self::FREQS.len();
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
pub struct NoteBlock {
    pub offset: Beats,
    pub value: Note,
    pub len: Beats
}

pub enum NoteBlockEvent {
    Add(R64, Note),
    Remove(usize),
    SetLen(usize, R64),
    Redraw
}

impl Graphable for NoteBlock {
    const EDITOR_NAME: &'static str = "Note Editor";
    // TODO: make this generic over the number of defined notes
    const Y_BOUND: Range<R64> = r64![0.0] .. r64![36.0];
    const SCALE_Y_BOUND: Range<R64> = r64![40.0] .. r64![40.0];
    const OFFSET_Y_BOUND: Range<R64> = r64![-2.0] .. r64![-2.0];
    const Y_SNAP: R64 = r64![1.0];
    type Inner = Beats;
    type Event = NoteBlockEvent;

    #[inline] fn inner(&self) -> &Self::Inner {&self.len}
    #[inline] fn inner_mut(&mut self) -> &mut Self::Inner {&mut self.len}

    #[inline] fn loc(&self) -> [R64; 2] {
        [self.offset, self.value.recip().index().into()]
    }

    #[inline] fn set_loc(&mut self, _n_points: usize, _self_id: usize, x: impl FnOnce() -> R64, y: impl FnOnce() -> R64)
    -> Option<Self::Event> {
        self.value = Note::from_index(y().into()).recip();
        let old = replace(&mut self.offset, x());
        (old != self.offset).then_some(NoteBlockEvent::Redraw)
    }

    fn draw(&self, _next: Option<&Self>, mapper: impl Fn([R64; 2]) -> [R64; 2]) -> JsResult<Path2d> {
        let res = Path2d::new().add_loc(loc!())?;
        let y: R64 = self.value.recip().index().into();
        let src = mapper([self.offset, y]);
        let dst = mapper([self.offset + self.len, y + 1u8]);
        res.rect(*src[0], *src[1], *dst[0] - *src[0], *dst[1] - *src[1]);
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

    #[inline] fn on_meta_click(loc: impl Fn() -> Option<[R64; 2]>) -> Option<Self::Event> {
        loc().map(|[x, y]| NoteBlockEvent::Add(x, Note::from_index(y.into()).recip()))
    }

    #[inline] fn on_meta_click_point<'a, F>(point: F) -> Option<Self::Event>
    where Self: 'a, F: Fn() -> SliceRef<'a, Self> {
        let p = point();
        (*p.len == 0.0).then_some(NoteBlockEvent::Remove(p.index()))
    }

    #[inline] fn on_meta_drag_point<'a, F1, F2>(point: F1, loc: F2) -> Option<Self::Event>
    where Self: 'a, F1: Fn() -> SliceRef<'a, Self>, F2: Fn() -> Option<[R64; 2]> {
        let p = point();
        loc().filter(|[x, _]| *x >= p.offset).map(|[x, _]| NoteBlockEvent::SetLen(p.index(), x - p.offset))
    }

    #[inline] fn in_hitbox(&self, point: [R64; 2]) -> bool {
        (self.value.recip().index() as f64).loose_eq(*point[1], 0.5)
            && (self.offset .. self.offset + self.len).contains(&point[0])
    }

    #[inline] fn fmt_loc(loc: Option<[R64; 2]>) -> String {
        if let Some([x, y]) = loc {
            format!("{x:.3}, {}", Note::from_index(y.into()).recip())
        } else {"--.---, --".to_owned()}
    }

    #[inline] fn meta_held_hint(loc: Option<[R64; 2]>, _: bool) -> Option<[Cow<'static, str>; 2]> {
        Some(["Note Editor: adding new note".into(), loc.map_or_default(|loc|
            format!("Click to add a point @ {}", Self::fmt_loc(Some(loc))).into())])
    }

    #[inline] fn meta_held_over_point_hint<'a, F>(point: F, left: bool) -> Option<[Cow<'static, str>; 2]>
    where Self: 'a, F: Fn() -> SliceRef<'a, Self> {
        let p = point();
        Some(["Note Editor: changing note length".into(),
            if left {format!("Length: {:.3}", p.len)}
            else {format!("Hold LMB to change length of note @ {}", Self::fmt_loc(Some(p.loc())))}.into()])
    }
}

#[derive(Default, Debug, Clone)]
pub enum Sound {
    #[default] None,
    Note{volume: R32, pattern: GraphEditor<NoteBlock>, release: Beats},
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
            SoundType::Note =>
                Self::Note{volume: r32![1.0], pattern: GraphEditor::new(vec![]), release: r64![0.2]},

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

            Sound::Note{pattern, ..} =>
                scheduler(SoundEvent::BlockStart{id: self_id, state: 0,
                    when: self_offset + unsafe{pattern.first_unchecked()}.offset}),

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

    pub fn poll(&mut self, plug: &AudioNode, ctx: &AppContext, src: SoundEvent, mut scheduler: impl FnMut(SoundEvent)) -> JsResult<()> {
        Ok(match self {
            Sound::None => (),

            Sound::Note{volume, pattern, release} => match src {
                SoundEvent::BlockStart{id, when, mut state} => {
                    let cur = unsafe{pattern.get_unchecked(state)};
                    let block_core = ctx.audio_ctx.create_oscillator().add_loc(loc!())?;
                    block_core.frequency().set_value(*cur.value.freq());
                    block_core.start().add_loc(loc!())?;
                    let block = ctx.audio_ctx.create_gain().add_loc(loc!())?;
                    {
                        let at = ctx.now + cur.len.to_secs(ctx.bps);
                        let gain = block.gain();
                        gain.set_value_at_time(**volume, 0.0).add_loc(loc!())?;
                        gain.set_value_at_time(**volume, *at).add_loc(loc!())?;
                        gain.linear_ramp_to_value_at_time(f32::MIN_POSITIVE, *at + *release.to_secs(ctx.bps)).add_loc(loc!())?;
                    }
                    block_core.connect_with_audio_node(&block).add_loc(loc!())?
                        .connect_with_audio_node(plug).add_loc(loc!())?;
                    scheduler(SoundEvent::BlockEnd{id, when: when + cur.len + *release + r64![0.1].secs_to_beats(ctx.bps), block});

                    state += 1;
                    if let Some(next) = pattern.get(state) {
                        scheduler(SoundEvent::BlockStart{id, when: when + next.offset - cur.offset, state})
                    }
                }

                SoundEvent::BlockEnd{block, ..} => block.disconnect().add_loc(loc!())?,

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
            Sound::Note{pattern, ..} =>
                unsafe{pattern.last_unchecked()}.pipe(|x| x.offset + x.len),
            Sound::Noise{len, ..} => *len
        }
    }

    #[inline] pub fn tabs(&self) -> &'static [TabInfo] {
        match self {
            Sound::None =>
                &[TabInfo{name: "Choose Sound Type"}],
            Sound::Note{..} =>
                &[TabInfo{name: "General"}, TabInfo{name: "Pattern"}],
            Sound::Noise{..} =>
                &[TabInfo{name: "General"}, TabInfo{name: "Volume"}]
        }
    }

    pub fn params(&self, ctx: &AppContext, setter: Callback<AppEvent>) -> Html {
        match self {
            Sound::None => html!{<div id="block-add-menu">
                {for Sound::TYPES.iter().map(|x| html!{
                    <Button name={x.name()}
                        setter={setter.reform(|_| AppEvent::SetBlockType(*x))}>
                        <p>{x.name()}</p>
                    </Button>
                })}
            </div>},

            Sound::Note{volume, pattern, release} => match ctx.selected_tab {
                0 /* General */ => html!{<div id="inputs">
                    <Slider key="note-vol"
                    setter={setter.reform(|x| AppEvent::Volume(R32::from(x)))}
                    name="Note Volume"
                    initial={*volume}/>
                    <Slider key="note-rel"
                    setter={setter.reform(AppEvent::Release)}
                    name="Note Release Time" postfix="Beats"
                    min={r64![0.1].secs_to_beats(ctx.bps)}
                    max={r64![3.0]}
                    initial={*release}/>
                </div>},
                1 /* Pattern */ => html!{
                    <canvas ref={pattern.canvas().clone()} class="blue-border"
                    onpointerdown={setter.reform(AppEvent::FocusTab)}
                    onpointerup={setter.reform(|e| AppEvent::HoverTab(MouseEvent::from(e)))}
                    onpointermove={setter.reform(|e| AppEvent::HoverTab(MouseEvent::from(e)))}
                    onpointerout={setter.reform(|_| AppEvent::LeaveTab)}
                    ondblclick={setter.reform(AppEvent::DoubleClickTab)}/>
                },
                tab_id => html!{<p style="color:red">{format!("Invalid tab ID: {tab_id}")}</p>}
            }

            Sound::Noise{len, gain, ..} => match ctx.selected_tab {
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
                Some(AppEvent::RedrawEditorPlane)
            } else {None}

            Sound::Note{volume, pattern, release} => match event {
                AppEvent::FocusTab(e) => {
                    e.target_dyn_into::<Element>().to_js_result(loc!())?
                        .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    pattern.handle_hover(Some(e.try_into().add_loc(loc!())?), ctx)
                        .add_loc(loc!())?;
                    None
                }

                AppEvent::HoverTab(e) => match pattern.handle_hover(Some(e.try_into().add_loc(loc!())?), ctx).add_loc(loc!())? {
                    Some(NoteBlockEvent::Add(offset, value)) => {
                        pattern.add_point(NoteBlock{offset, value, len: r64![1.0]});
                        Some(AppEvent::RedrawEditorPlane)
                    }

                    Some(NoteBlockEvent::Remove(id)) => {
                        pattern.del_point(id).add_loc(loc!())?;
                        Some(AppEvent::RedrawEditorPlane)
                    }

                    Some(NoteBlockEvent::SetLen(id, len)) => {
                        *unsafe{pattern.get_unchecked_mut(id)}.inner() = len;
                        pattern.force_redraw();
                        Some(AppEvent::RedrawEditorPlane)
                    }

                    Some(NoteBlockEvent::Redraw) => Some(AppEvent::RedrawEditorPlane),

                    None => None
                }

                AppEvent::LeaveTab => pattern
                    .handle_hover(None, ctx).add_loc(loc!())?
                    .pipe(|_| None),

                AppEvent::AfterSetTab(1) => pattern
                    .init(|c| Ok([c.client_width() as u32, c.client_height() as u32]))
                    .add_loc(loc!())?.pipe(|_| None),

                AppEvent::Frame(_) if ctx.selected_tab == 1 => pattern
                    .redraw(ctx).add_loc(loc!())?
                    .map(|[m, a]| AppEvent::SetHint(m, a)),

                AppEvent::Volume(value) => {
                    *volume = *value;
                    None
                }

                AppEvent::Release(value) => {
                    *release = *value;
                    None
                }

                AppEvent::AudioStarted(_) => pattern.force_redraw()
                    .pipe(|_| None),

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

#[derive(Debug)]
pub struct SoundBlock {
    pub sound: Sound,
    pub layer: i32,
    pub offset: Beats
}

impl PartialEq for SoundBlock {
    #[inline] fn eq(&self, other: &Self) -> bool {
        self.offset.eq(&other.offset)
    }
}

impl Eq for SoundBlock {}

impl PartialOrd for SoundBlock {
    #[inline] fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.offset.partial_cmp(&other.offset)
    }
}

impl Ord for SoundBlock {
    #[inline] fn cmp(&self, other: &Self) -> Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl Graphable for SoundBlock {
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

    fn draw(&self, _: Option<&Self>, mapper: impl Fn([R64; 2]) -> [R64; 2]) -> JsResult<Path2d> {
        let res = Path2d::new().add_loc(loc!())?;
        let src = mapper([self.offset, self.layer.into()]);
        let dst = mapper([self.offset + self.sound.len(), (self.layer + 1).into()]);
        res.rect(*src[0], *src[1], *dst[0] - *src[0], *dst[1] - *src[1]);
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
            format!("{x:.3}, layer {y:.0}")
        } else {"--.---, layer --".to_owned()}
    }

    #[inline] fn on_select(self_id: Option<usize>) -> Option<Self::Event> {
        Some(AppEvent::Select(self_id))
    }

    #[inline] fn on_meta_click_point<'a, F>(point: F) -> Option<Self::Event>
    where Self: 'a, F: Fn() -> SliceRef<'a, Self> {
        Some(AppEvent::Select(Some(point().index())))
    }

    #[inline] fn on_meta_click(loc: impl Fn() -> Option<[R64; 2]>) -> Option<Self::Event> {
        loc().map(|[x, y]| AppEvent::Add(y.into(), x))
    }

    #[inline] fn meta_held_hint(loc: Option<[R64; 2]>, _: bool) -> Option<[Cow<'static, str>; 2]> {
        Some(["Editor plane: adding new block".into(), loc.map_or_default(|[x, y]|
            format!("Click to add a block at {x:.3}, layer {y:.0}").into())])
    }

    #[inline] fn meta_held_over_point_hint<'a, F>(point: F, _: bool) -> Option<[Cow<'static, str>; 2]>
    where Self: 'a, F: Fn() -> SliceRef<'a, Self> {
        Some(["Editor plane: selecting block".into(),
            format!{"Click to select point @ {}", Self::fmt_loc(Some(point().loc()))}.into()])
    }
}

/// all the `when` fields are offsets from the start in beats
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SoundEvent {
    BlockStart{id: usize, when: Beats, state: usize},
    BlockEnd{id: usize, when: Beats, block: GainNode},
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
            | SoundEvent::BlockStart{id, ..}
            | SoundEvent::BlockEnd{id, ..} => *id
        }
    }

    #[inline] fn when(&self) -> Secs {
        match self {
            SoundEvent::Start{when, ..}
            | SoundEvent::Stop{when, ..}
            | SoundEvent::BlockStart{when, ..} 
            | SoundEvent::BlockEnd{when, ..} => *when,
        }
    }
}

pub struct Sequencer {
    pattern: GraphEditor<SoundBlock>,
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

        plug.connect_with_audio_node(&gain).add_loc(loc!())?
            .connect_with_audio_node(&visualiser).add_loc(loc!())?
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

    #[inline] pub fn pattern_mut(&mut self) -> &mut GraphEditor<SoundBlock> {
        &mut self.pattern
    }

    #[inline] pub fn pattern(&self) -> &GraphEditor<SoundBlock> {
        &self.pattern
    }

    pub fn handle_event(&mut self, event: &AppEvent, ctx: &AppContext) -> JsResult<Option<AppEvent>> {
        Ok(match event {
            &AppEvent::Add(layer, offset) => self.pattern
                .add_point(SoundBlock{sound: Sound::default(), layer, offset})
                .pipe(|_| None),

            AppEvent::Select(mut id) => {
                id = id.filter(|x| Some(*x) != self.pattern.fixed_id());
                self.pattern.fix_point(id).add_loc(loc!())?;
                None
            }

            AppEvent::Remove => self.pattern
                .del_fixed().to_js_result(loc!())?.pipe(|_| None),

            AppEvent::StartPlay => {
                self.pending.clear();
                for (id, mut block) in self.pattern.iter_mut().enumerate() {
                    let offset = block.offset;
                    block.inner().reset(ctx, id, offset,
                        |x| _ = self.pending.push_sorted(x)).add_loc(loc!())?;
                }
                self.playing = true;
                None
            }

            AppEvent::StopPlay => {
                self.pending.clear();
                self.plug.disconnect().add_loc(loc!())?;
                self.plug = ctx.audio_ctx.create_dynamics_compressor().add_loc(loc!())?;
                self.plug.ratio().set_value(20.0);
                self.plug.release().set_value(1.0);
                self.plug.connect_with_audio_node(&self.gain).add_loc(loc!())?;
                self.playing = false;
                self.used_to_play = false;
                None
            }

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
                    let mut ctx = Cow::Borrowed(ctx);
                    let (to_emit, now) = if self.used_to_play {
                        (None, (ctx.now - ctx.play_since).secs_to_beats(ctx.bps))
                    } else {
                        ctx.to_mut().play_since = ctx.now;
                        self.used_to_play = true;
                        self.pattern.force_redraw();
                        (Some(AppEvent::AudioStarted(ctx.now)), r64![0.0])
                    };
                    let n_due = self.pending.iter().position(|x| x.when() > now).unwrap_or(self.pending.len());
                    for event in self.pending.drain(..n_due).collect::<Vec<_>>() {
                        let id = event.target();
                        let mut block = unsafe{self.pattern.get_unchecked_mut(id)};
                        let mut due_now = vec![event];

                        while !due_now.is_empty() {
                            for event in due_now.take() {
                                block.inner().poll(&self.plug, &ctx, event, |new| if new.when() > now {
                                    self.pending.push_sorted(new);
                                } else {
                                    due_now.push(new);
                                }).add_loc(loc!())?;
                            }
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
