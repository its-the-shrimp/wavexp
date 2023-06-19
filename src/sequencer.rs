use std::{
    cmp::Ordering,
    ops::Range};
use wasm_bindgen::JsCast;
use web_sys::{AnalyserNode as JsAnalyserNode, DynamicsCompressorNode, GainNode, AudioContext, Path2d, HtmlCanvasElement, HtmlElement, Element, DragEvent};
use yew::{NodeRef, Html, html, TargetCast, Callback};
use crate::{
    sound::{Secs, Sound, Beats, FromBeats, SoundType},
    utils::{JsResult, JsResultUtils, R64, VecExt, R32, OptionExt, RatioToInt, Pipe, document, HtmlDocumentExt, SliceExt},
    input::{AppEvent, Switch},
    visual::{GraphEditor, Graphable, GraphPointView},
    r64, loc
};

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

    #[inline] fn set_loc(&mut self, _: usize, _: usize, x: impl FnOnce() -> R64, y: impl FnOnce() -> R64) {
        self.offset = x();
        self.layer = y().to_int();
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
        format!("{:.3}, layer {}", loc[0], *loc[1] as i32)
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
        res.move_to(0.0, -*step[1]);
        res.move_to(0.0,  *step[1]);
        res.move_to(*x1, *y2);
        res.line_to(*x2, *y2);
        Ok(res)
    }
}

pub enum SequencerState {
    Start,
    Play{next: usize, start_time: Secs},
    Idle{start_time: Secs},
    Stop,
    None
}

pub struct Sequencer {
    pattern: GraphEditor<PatternBlock>,
    pending: Vec<(usize, Secs)>,
    state: SequencerState,
    audio_ctx: AudioContext,
    visualiser: JsAnalyserNode,
    plug: DynamicsCompressorNode,
    gain: GainNode,
    bps: Beats,
    selected_id: Option<usize>
}

impl Sequencer {
    #[inline] pub fn new() -> JsResult<Self> {
        let audio_ctx = AudioContext::new().add_loc(loc!())?;
        let plug = DynamicsCompressorNode::new(&audio_ctx).add_loc(loc!())?;
        plug.ratio().set_value(20.0);
        plug.release().set_value(1.0);
        let gain = GainNode::new(&audio_ctx).add_loc(loc!())?;
        gain.gain().set_value(0.2);
        let visualiser = JsAnalyserNode::new(&audio_ctx).add_loc(loc!())?;
        visualiser.set_fft_size(512);

        plug.connect_with_audio_node(&visualiser).add_loc(loc!())?
            .connect_with_audio_node(&gain).add_loc(loc!())?
            .connect_with_audio_node(&audio_ctx.destination()).add_loc(loc!())?;

        Ok(Self{pattern: GraphEditor::new(r64![20.0], r64![10.0], vec![]), pending: vec![], audio_ctx,
            state: SequencerState::None, visualiser, plug, gain, bps: r64![2.0],
            selected_id: None})
    }

    #[inline] pub fn visualiser(&self) -> &JsAnalyserNode {
        &self.visualiser
    }

    #[inline] pub fn gain(&self) -> R32 {
        R32::new_or(R32::ZERO, self.gain.gain().value())
    }

    /// Beats Per Second
    #[inline] pub fn bps(&self) -> Beats {
        self.bps
    }

    #[inline] pub fn canvas(&self) -> &NodeRef {
        self.pattern.canvas()
    }

    #[inline] pub fn selected_block(&self) -> Option<(usize, &PatternBlock)> {
        self.selected_id.map(|x|
            (x, unsafe{self.pattern.get_unchecked(x)}))
    }

    #[inline] pub fn selected_block_mut(&mut self) -> Option<(usize, GraphPointView<PatternBlock>)> {
        self.selected_id.map(|x|
            (x, unsafe{self.pattern.get_unchecked_mut(x)}))
    }

    #[inline] pub fn pattern_mut(&mut self) -> &mut GraphEditor<PatternBlock> {
        &mut self.pattern
    }

    pub fn editor_plane_params(&self, setter: Callback<AppEvent>) -> Html {
        html!{
            <div id="plane-settings" data-main-hint="Editor plane settings">
                <Switch key="snap" name="Interval for blocks to snap to"
                setter={setter.reform(|x: usize|
                    AppEvent::SnapStep(*[r64![0.0], r64![1.0], r64![0.5], r64![0.25], r64![0.125]].get_wrapping(x)))}
                options={vec!["None", "1", "1/2", "1/4", "1/8"]}
                initial={match *self.pattern.snap_step() {
                    x if x == 1.0   => 1,
                    x if x == 0.5   => 2,
                    x if x == 0.25  => 3,
                    x if x == 0.125 => 4,
                    _ => 0
                }}/>
            </div>
        }
    }

    pub fn handle_event(&mut self, event: AppEvent) -> JsResult<Option<AppEvent>> {
        Ok(match event {
            AppEvent::Add(ty, layer, offset) => self.pattern.add_point(PatternBlock{
                sound: Sound::new(ty, &self.audio_ctx).add_loc(loc!())?,
                layer, offset})
                .pipe(|_| None),

            AppEvent::Remove(id) => self.pattern.del_point(id).add_loc(loc!())?
                .pipe(|_| None),

            AppEvent::TogglePlay => if matches!(self.state, SequencerState::Start |SequencerState::Play{..}) {
                self.state = SequencerState::Start;
                for mut block in self.pattern.iter_mut() {
                    block.inner().reset(&self.audio_ctx).add_loc(loc!())?;
                }
            } else {
                self.state = SequencerState::Stop;
            }.pipe(|_| None),

            AppEvent::Select(id) => {
                self.selected_id = id;
                Some(AppEvent::SetTab(0))
            }

            AppEvent::SetTab(tab_id) => if let Some(id) = self.selected_id {
                unsafe{self.pattern.get_unchecked_mut(id)}.inner()
                    .handle_event(AppEvent::SetTab(tab_id)).add_loc(loc!())?;
            }.pipe(|_| None),

            AppEvent::SnapStep(value) => self.pattern.set_snap_step(value)
                .pipe(|_| None),

            AppEvent::Bpm(value) => {
                self.bps = value / 60u8;
                None
            }

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
                if let Some(id) = self.selected_id {
                    unsafe{self.pattern.get_unchecked_mut(id)}.inner()
                        .handle_event(AppEvent::Resize).add_loc(loc!())?;
                }
                None
            }

            AppEvent::FocusPlane(e) => {
                e.target_dyn_into::<Element>().to_js_result(loc!())?
                    .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                self.pattern.handle_hover(Some(e.try_into().add_loc(loc!())?))
            }

            AppEvent::HoverPlane(e) => {
                if let Some(e) = e.dyn_ref::<DragEvent>() {
                    e.prevent_default();
                }
                self.pattern.handle_hover(Some(e.try_into().add_loc(loc!())?))
            }

            AppEvent::LeavePlane => self.pattern.handle_hover(None),

            AppEvent::SetBlockAdd(d) => {
                self.pattern.set_draggable(d);
                None
            }

            AppEvent::MasterGain(value) => self.gain.gain()
                .set_value(*value).pipe(|_| None),

            event => if let Some(block_id) = event.block_id() {
                self.pattern.get_mut(block_id).to_js_result(loc!())?
                    .inner().handle_event(event).add_loc(loc!())?;
            }.pipe(|_| None)
        })
    }

    #[inline] pub fn play_offset(&self, time: Secs) -> Beats {
        use SequencerState::*;
        if let Play{start_time, ..} | Idle{start_time} = self.state {
            (time - start_time).secs_to_beats(self.bps)
        } else {Beats::NEG_INFINITY}
    }

    pub fn poll(&mut self, time: Secs) -> JsResult<()> {
        match self.state {
            SequencerState::Start => {
                let mut next = 0;
                for mut block in self.pattern.iter_mut().take_while(|x| *x.offset == 0.0) {
                    let when = block.inner().poll(time, &self.plug, self.bps).add_loc(loc!())?;
                    self.pending.push_sorted_by_key((next, when), |x| x.1);
                    next += 1;
                }
                self.state = SequencerState::Play{next, start_time: time};
            }

            SequencerState::Play{ref mut next, start_time} => {
                let offset = (time - start_time).secs_to_beats(self.bps);
                for mut block in self.pattern.iter_mut().skip(*next).take_while(|x| x.offset <= offset) {
                    let when = block.inner().poll(time, &self.plug, self.bps).add_loc(loc!())?;
                    self.pending.push_sorted_by_key((*next, when), |x| x.1);
                    *next += 1;
                }
                if *next >= self.pattern.data().len() {
                    self.state = SequencerState::Idle{start_time};
                }
            }

            SequencerState::Stop => {
                let mut err = Ok(());
                self.pending.drain_filter(|(id, when)| unsafe {
                    *when = self.pattern.get_unchecked_mut(*id)
                        .inner().stop(time, self.bps).add_loc(loc!())
                        .unwrap_or_else(|x| {err = Err(x); R64::INFINITY});
                    when.is_finite()
                });
                err?;
                self.state = SequencerState::None;
            }

            SequencerState::Idle{..} | SequencerState::None => ()
        }

        let n_due = self.pending.iter().position(|x| x.1 > time).unwrap_or(self.pending.len());
        let due: Vec<usize> = self.pending.drain(..n_due).map(|x| x.0).collect::<Vec<_>>();
        for id in due {
            let mut block = unsafe{self.pattern.get_unchecked_mut(id)};
            let when = block.inner().poll(time, &self.plug, self.bps).add_loc(loc!())?;
            if when.is_infinite() {continue}
            self.pending.push_sorted_by_key((id, when), |x| x.0);
        }
        Ok(())
    }
}
