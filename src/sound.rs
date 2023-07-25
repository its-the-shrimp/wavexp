use std::{
    ops::{Add, Sub, Range, AddAssign, SubAssign, Deref, Not},
    fmt::{self, Display, Formatter, Debug},
    cmp::Ordering,
    rc::Rc,
    borrow::Cow, cell::{RefCell, LazyCell}};
use js_sys::Math::random;
use web_sys::{
    AudioNode,
    AudioContext,
    AudioBufferSourceNode,
    AudioBuffer,
    GainNode,
    Path2d, DynamicsCompressorNode, AnalyserNode, HtmlCanvasElement};
use yew::{
    html,
    Html,
    Callback,
    scheduler::Shared};
use crate::{
    utils::{
        JsResult,
        JsResultUtils,
        R64, R32,
        LooseEq, OptionExt, Pipe, document, VecExt, js_error, Take, default, ResultToJsResult, report_err},
    input::{Slider, Button, Buttons, Cursor},
    visual::{GraphEditor, Graphable, GraphEditorCanvas},
    global::{AppContext, AppEvent, AppAction},
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
        Self((self.0 as usize).saturating_add_signed(rhs).min(Self::N_NOTES) as u8)
    }
}

impl AddAssign<isize> for Note {
    #[inline] fn add_assign(&mut self, rhs: isize) {
        self.0 = (self.0 as usize).saturating_add_signed(rhs).min(Self::N_NOTES) as u8;
    }
}

impl Sub<isize> for Note {
    type Output = Note;
    #[inline] fn sub(self, rhs: isize) -> Self::Output {
        Self((self.0 as isize - rhs).clamp(0, Self::N_NOTES as isize) as u8)
    }
}

impl SubAssign<isize> for Note {
    #[inline] fn sub_assign(&mut self, rhs: isize) {
        self.0 = (self.0 as isize - rhs).clamp(0, Self::N_NOTES as isize) as u8;
    }
}

impl Note {
    pub const MAX: Note = Note(35);
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

impl PartialOrd for NoteBlock {
    #[inline] fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.offset.partial_cmp(&other.offset)
    }
}

impl Ord for NoteBlock {
    #[inline] fn cmp(&self, other: &Self) -> Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl Graphable for NoteBlock {
    const EDITOR_NAME: &'static str = "Note Editor";
    // TODO: make this generic over the number of defined notes
    const Y_BOUND: Range<R64> = r64![0.0] .. r64![36.0];
    const SCALE_Y_BOUND: Range<R64> = r64![40.0] .. r64![40.0];
    const OFFSET_Y_BOUND: Range<R64> = r64![-2.0] .. r64![-2.0];
    const Y_SNAP: R64 = r64![1.0];
    type Inner = Beats;

    #[inline] fn inner(&self) -> &Self::Inner {&self.len}
    #[inline] fn inner_mut(&mut self) -> &mut Self::Inner {&mut self.len}

    #[inline] fn loc(&self) -> [R64; 2] {
        [self.offset, self.value.recip().index().into()]
    }

    #[inline] fn move_point(point: Result<&mut Self, &mut [R64; 2]>, delta: [R64; 2], meta: bool) {
        match point {
            Ok(NoteBlock{offset, value, len}) => {
                if meta {
                    *len += delta[0];
                } else {
                    *offset = r64![0.0].max(*offset + delta[0]);
                }
                *value -= delta[1].into();
            }
            Err(point) => {
                if !meta {
                    point[0] += delta[0];
                }
                point[1] -= delta[1];
            }
        }
    }
    fn draw(&self, _next: Option<&Self>, mapper: impl Fn([R64; 2]) -> [R64; 2]) -> JsResult<Path2d> {
        let res = Path2d::new().add_loc(loc!())?;
        let y: R64 = self.value.recip().index().into();
        let src = mapper([self.offset, y]);
        let dst = mapper([self.offset + self.len.max(R64::ZERO), y + 1u8]);
        res.rect(*src[0], *src[1], *dst[0] - *src[0], *dst[1] - *src[1]);
        Ok(res)
    }

    #[inline] fn in_hitbox(&self, point: [R64; 2]) -> bool {
        (self.value.recip().index() as f64).loose_eq(*point[1], 0.5)
            && (self.offset .. self.offset + self.len).contains(&point[0])
    }

    #[inline] fn fmt_loc(loc: [R64; 2]) -> String {
        format!("{:.3}, {}", loc[0], Note::from_index(loc[1].into()).recip())
    }

    #[inline] fn on_move(editor: &mut GraphEditor<Self>, ctx: &AppContext, _: Cursor, _: [R64; 2]) {
        if editor.selection().contains(&(editor.len() - 1)) {
            ctx.emit_event(AppEvent::RedrawEditorPlane)
        }
    }

    #[inline] fn on_click(
        editor: &mut GraphEditor<Self>,
        ctx: &AppContext,
        cursor: Cursor,
        pressed_at:    impl Deref<Target = [R64; 2]>,
        released_at:   impl Deref<Target = [R64; 2]>,
        old_selection: Option<&[usize]>
    ) -> Option<AppAction> {
        if cursor.meta && editor.selection().is_empty() && old_selection.map_or(true, |x| x.is_empty()) && *pressed_at == *released_at {
            let [offset, y] = *released_at;
            let point_id = editor.add_point(Self{offset, value: Note::from_index(y.into()).recip(), len: r64![1.0]});
            ctx.emit_event(AppEvent::RedrawEditorPlane);
            Some(AppAction::AddPoint{editor_id: editor.id(), point_id})
        } else {
            let mut sel = editor.selection().to_owned();
            sel.retain(|i| *unsafe{editor.get_unchecked(*i)}.len > 0.0);
            if !sel.is_empty() {
                _ = editor.remove_points(&sel).report_err(loc!());
                ctx.emit_event(AppEvent::RedrawEditorPlane)
            }
            None
        }
    }

    #[inline] fn on_plane_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &AppContext,
        cursor: Cursor,
        _: impl Deref<Target = [R64; 2]>,
        first: bool
    ) {
        if !first && Some(&*cursor) == editor.last_cursor().as_deref() {return}
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
        ctx.emit_event(AppEvent::SetHint(m, a))
    }

    #[inline] fn on_point_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &AppContext,
        cursor: Cursor,
        point_id: usize,
        first: bool
    ) {
        if !first {return}
        let m = Self::fmt_loc(unsafe{editor.get_unchecked(point_id)}.loc());
        let [m, a] = match *cursor {
            Buttons{left: false, meta: false, ..} =>
                [m.into(), "Hold & drag to move around (press Meta for actions)".into()],
            Buttons{left: false, meta: true, ..} =>
                [m.into(), "Click to add note, hold & drag to select".into()],
            Buttons{left: true, meta: false, ..} =>
                [Cow::from(m) + ": Moving", "Release to stop".into()],
            Buttons{left: true, meta: true, ..} =>
                [Cow::from(m) + ": Selecting", "Release to select".into()]
        };
        ctx.emit_event(AppEvent::SetHint(m, a))
    }

    #[inline] fn on_selection_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &AppContext,
        cursor: Cursor,
        first: bool
    ) {
        if !first {return}
        let m = editor.selection().len().pipe(|l| format!("{l} note{}", if l == 1 {""} else {"s"}));
        let [m, a] = match *cursor {
            Buttons{left: false, meta: false, ..} =>
                [m.into(), "LMB to move, LMB + Meta to stretch".into()],
            Buttons{left: false, meta: true, ..} =>
                [m.into(), "Hold LMB to stretch it".into()],
            Buttons{left: true, meta: false, ..} =>
                [(m + ": moving").into(), "Release to stop".into()],
            Buttons{left: true, meta: true, ..} =>
                [(m + ": stretching").into(), "Release to stop".into()],
        };
        ctx.emit_event(AppEvent::SetHint(m, a))
    }
}

#[derive(Default, Debug, Clone)]
pub enum Sound {
    #[default] None,
    Note{volume: R32, pattern: Shared<GraphEditor<NoteBlock>>,
        attack: Beats, decay: Beats, sustain: R32, release: Beats},
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
                Self::Note{volume: r32![1.0], pattern: Rc::new(RefCell::new(GraphEditor::new(vec![]))),
                    attack: r64![0.0], decay: r64![0.0], sustain: r32![1.0], release: r64![0.2]},

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
            Sound::Noise{..} => "White Noise"
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
                    when: self_offset
                        + unsafe{pattern.try_borrow().to_js_result(loc!())?.first_unchecked()}.offset}),

            Sound::Noise{gen, src, gain, ..} => {
                gen.disconnect().add_loc(loc!())?;
                *gen = ctx.audio_ctx().create_buffer_source().add_loc(loc!())?;
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

            Sound::Note{volume, pattern, attack, decay, mut sustain, release} => match src {
                SoundEvent::BlockStart{id, when, mut state} => {
                    let pattern = pattern.try_borrow().to_js_result(loc!())?;
                    let cur = unsafe{pattern.get_unchecked(state)};
                    let block_core = ctx.audio_ctx().create_oscillator().add_loc(loc!())?;
                    block_core.frequency().set_value(*cur.value.freq());
                    block_core.start().add_loc(loc!())?;
                    let block = ctx.audio_ctx().create_gain().add_loc(loc!())?;
                    {
                        let mut at = ctx.now();
                        let gain = block.gain();
                        gain.set_value_at_time(f32::MIN_POSITIVE, *at).add_loc(loc!())?;
                        at += attack.to_secs(ctx.bps());
                        gain.linear_ramp_to_value_at_time(**volume, *at).add_loc(loc!())?;
                        at += decay.to_secs(ctx.bps());
                        sustain *= *volume;
                        gain.linear_ramp_to_value_at_time(*sustain, *at).add_loc(loc!())?;
                        at = ctx.now() + cur.len.to_secs(ctx.bps());
                        gain.set_value_at_time(*sustain, *at).add_loc(loc!())?;
                        at += release.to_secs(ctx.bps());
                        gain.linear_ramp_to_value_at_time(f32::MIN_POSITIVE, *at).add_loc(loc!())?;
                    }
                    block_core.connect_with_audio_node(&block).add_loc(loc!())?
                        .connect_with_audio_node(plug).add_loc(loc!())?;
                    scheduler(SoundEvent::BlockEnd{id, when: when + cur.len + *release + r64![0.1].secs_to_beats(ctx.bps()), block});

                    state += 1;
                    if let Some(next) = pattern.get(state) {
                        scheduler(SoundEvent::BlockStart{id, when: when + next.offset - cur.offset, state})
                    }
                }

                SoundEvent::BlockEnd{block, ..} => block.disconnect().add_loc(loc!())?,

                src => js_error(format!("invalid event: {src:?}"), loc!())?,
            }

            Sound::Noise{gain, len, ..} => match src {
                SoundEvent::Start{id, when} => {
                    gain.connect_with_audio_node(plug).add_loc(loc!())?;
                    scheduler(SoundEvent::Stop{id, when: when + *len});
                }

                SoundEvent::Stop{..} => gain.disconnect().add_loc(loc!())?,

                src => js_error(format!("invalid event: {src:?}"), loc!())?,
            }
        })
    }

    #[inline] pub fn len(&self) -> Beats {
        match self {
            Sound::None => r64![1.0],
            Sound::Note{pattern, ..} => match pattern.try_borrow().to_js_result(loc!()) {
                Ok(p) => unsafe{p.last_unchecked()}.pipe(|x| x.offset + x.len),
                Err(err) => {report_err(err); default()}
            }
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

            Sound::Note{volume, pattern, attack, decay, sustain, release} => match ctx.selected_tab() {
                0 /* General */ => html!{<div id="inputs">
                    <Slider key="note-att"
                    setter={setter.reform(AppEvent::Attack)}
                    name="Note Attack Time" postfix="Beats"
                    max={r64![3.0]}
                    initial={*attack}/>
                    <Slider key="note-dec"
                    setter={setter.reform(AppEvent::Decay)}
                    name="Note Decay Time" postfix="Beats"
                    max={r64![3.0]}
                    initial={*decay}/>
                    <Slider key="note-sus"
                    setter={setter.reform(|x| AppEvent::Sustain(R32::from(x)))}
                    name="Note Sustain Level"
                    initial={*sustain}/>
                    <Slider key="note-rel"
                    setter={setter.reform(AppEvent::Release)}
                    name="Note Release Time" postfix="Beats"
                    min={r64![0.1].secs_to_beats(ctx.bps())}
                    max={r64![3.0]}
                    initial={*release}/>
                    <Slider key="note-vol"
                    setter={setter.reform(|x| AppEvent::Volume(R32::from(x)))}
                    name="Note Volume"
                    initial={*volume}/>
                </div>},
                1 /* Pattern */ => html!{
                    <GraphEditorCanvas<NoteBlock> editor={pattern} emitter={setter}/>
                },
                tab_id => html!{<p style="color:red">{format!("Invalid tab ID: {tab_id}")}</p>}
            }

            Sound::Noise{len, gain, ..} => match ctx.selected_tab() {
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

    pub fn handle_event(&mut self, event: &AppEvent, ctx: &mut AppContext) -> JsResult<()> {
        Ok(match self {
            Sound::None => if let AppEvent::SetBlockType(ty) = event {
                *self = Self::new(*ty, ctx.audio_ctx()).add_loc(loc!())?;
                ctx.emit_event(AppEvent::RedrawEditorPlane)
            }

            Sound::Note{volume, pattern, attack, decay, sustain, release} => match event {
                AppEvent::AfterSetTab(1) => pattern.try_borrow_mut().to_js_result(loc!())?
                    .init().add_loc(loc!())?,

                AppEvent::Volume(value)  =>  *volume = *value,
                AppEvent::Attack(value)  =>  *attack = *value,
                AppEvent::Decay(value)   =>   *decay = *value,
                AppEvent::Sustain(value) => *sustain = *value,
                AppEvent::Release(value) => *release = *value,

                e => if ctx.selected_tab() == 1 {
                    pattern.try_borrow_mut().to_js_result(loc!())?
                        .handle_event(e, ctx).add_loc(loc!())?
                }
            }

            Sound::Noise{len, gain, ..} => match event {
                AppEvent::Duration(value) => {
                    *len = *value;
                    ctx.emit_event(AppEvent::RedrawEditorPlane)
                }

                AppEvent::Volume(value) => gain.gain().set_value(**value),

                _ => ()
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

    #[inline] fn inner(&self) -> &Self::Inner {&self.sound}
    #[inline] fn inner_mut(&mut self) -> &mut Self::Inner {&mut self.sound}

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

    fn draw(&self, _: Option<&Self>, mapper: impl Fn([R64; 2]) -> [R64; 2]) -> JsResult<Path2d> {
        let res = Path2d::new().add_loc(loc!())?;
        let src = mapper([self.offset, self.layer.into()]);
        let dst = mapper([self.offset + self.sound.len(), (self.layer + 1).into()]);
        res.rect(*src[0], *src[1], *dst[0] - *src[0], *dst[1] - *src[1]);
        Ok(res)
    }

    #[inline] fn in_hitbox(&self, point: [R64; 2]) -> bool {
        self.layer == *point[1] as i32
            && (self.offset .. self.offset + self.sound.len().max(r64![0.1]))
                .contains(&point[0])
    }

    #[inline] fn fmt_loc(loc: [R64; 2]) -> String {
        format!("{:.3}, layer {}", loc[0], loc[1].floor())
    }

    #[inline] fn on_click(
        editor: &mut GraphEditor<Self>,
        ctx: &AppContext,
        cursor: Cursor,
        pressed_at:    impl Deref<Target = [R64; 2]>,
        released_at:   impl Deref<Target = [R64; 2]>,
        old_selection: Option<&[usize]>
    ) -> Option<AppAction> {
        let sel_is_empty = LazyCell::new(|| editor.selection().is_empty());
        if cursor.meta && *sel_is_empty && old_selection.map_or(true, |x| x.is_empty()) && *pressed_at == *released_at {
            let [offset, y] = *released_at;
            let point_id = editor.add_point(SoundBlock{sound: default(), layer: y.into(), offset});
            Some(AppAction::AddPoint{editor_id: editor.id(), point_id})
        } else {
            ctx.emit_event(AppEvent::Select(sel_is_empty.not().then_some(0)));
            None
        }
    }

    #[inline] fn on_plane_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &AppContext,
        cursor: Cursor,
        _: impl Deref<Target = [R64; 2]>,
        first: bool
    ) {
        if !first && Some(&*cursor) == editor.last_cursor().as_deref() {return}
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
        ctx.emit_event(AppEvent::SetHint(m, a))
    }

    #[inline] fn on_point_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &AppContext,
        cursor: Cursor,
        point_id: usize,
        first: bool
    ) {
        let m = || unsafe{editor.get_unchecked(point_id)}.desc().into();
        let [m, a] = if cursor.left {
            [m() + ": moving", "Release to stop".into()]
        } else if first {
            [m(), "Hold & drag to move".into()]
        } else {return};
        ctx.emit_event(AppEvent::SetHint(m, a))
    }

    #[inline] fn on_selection_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &AppContext,
        cursor: Cursor,
        first: bool
    ) {
        if !first {return}
        let m = editor.selection().len().pipe(|l| format!("{l} block{}", if l == 1 {""} else {"s"}));
        let [m, a] = if cursor.left {
            [(m + ": moving").into(), "Release to stop".into()]
        } else {
            [m.into(), "Hold & drag to move".into()]
        };
        ctx.emit_event(AppEvent::SetHint(m, a))
    }

    #[inline] fn canvas_coords(canvas: &HtmlCanvasElement) -> JsResult<[u32; 2]> {
        let doc = document();
        let w = doc.body().to_js_result(loc!())?.client_width()
            - canvas.previous_element_sibling().to_js_result(loc!())?
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
    pattern: Shared<GraphEditor<SoundBlock>>,
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

        Ok(Self{plug, gain, pattern: Rc::new(RefCell::new(GraphEditor::new(vec![]))), pending: vec![],
            playing: false, used_to_play: false})
    }

    #[inline] pub fn gain(&self) -> R32 {
        R32::new_or(R32::ZERO, self.gain.gain().value())
    }

    #[inline] pub fn pattern(&self) -> &Shared<GraphEditor<SoundBlock>> {
        &self.pattern
    }

    pub fn handle_event(&mut self, event: &AppEvent, ctx: &mut AppContext) -> JsResult<()> {
        match event {
            AppEvent::StartPlay => {
                self.pending.clear();
                let mut pattern = self.pattern.try_borrow_mut().to_js_result(loc!())?;
                for (id, mut block) in pattern.iter_mut().enumerate() {
                    let offset = block.offset;
                    block.inner().reset(ctx, id, offset,
                        |x| _ = self.pending.push_sorted(x)).add_loc(loc!())?;
                }
                self.playing = true;
            }

            AppEvent::StopPlay => {
                self.pending.clear();
                self.plug.disconnect().add_loc(loc!())?;
                self.plug = ctx.audio_ctx().create_dynamics_compressor().add_loc(loc!())?;
                self.plug.ratio().set_value(20.0);
                self.plug.release().set_value(1.0);
                self.plug.connect_with_audio_node(&self.gain).add_loc(loc!())?;
                self.playing = false;
                self.used_to_play = false;
            }

            AppEvent::RedrawEditorPlane => self.pattern
                .try_borrow_mut().to_js_result(loc!())?
                .force_redraw(),

            AppEvent::MasterGain(value) => self.gain
                .gain().set_value(**value),

            AppEvent::Frame(_) => {
                let mut pattern = self.pattern.try_borrow_mut().to_js_result(loc!())?;
                if self.playing {
                    let (ctx, now) = if self.used_to_play {
                        (Cow::Borrowed(ctx), (ctx.now() - ctx.play_since()).secs_to_beats(ctx.bps()))
                    } else {
                        self.used_to_play = true;
                        pattern.force_redraw();
                        ctx.emit_event(AppEvent::AudioStarted(ctx.now()));
                        (Cow::Owned(ctx.clone().set_play_since(ctx.now())), r64![0.0])
                    };
                    let n_due = self.pending.iter().position(|x| x.when() > now).unwrap_or(self.pending.len());
                    for event in self.pending.drain(..n_due).collect::<Vec<_>>() {
                        let id = event.target();
                        let mut block = unsafe{pattern.get_unchecked_mut(id)};
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
                }
                pattern.handle_event(event, ctx).add_loc(loc!())?
            }

            e => self.pattern().try_borrow_mut().to_js_result(loc!())?
                .handle_event(e, ctx).add_loc(loc!())?
        }
        Ok(())
    }
}
