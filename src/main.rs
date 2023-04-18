#![feature(try_blocks)]
#![feature(never_type)]
#![feature(unwrap_infallible)]
#![feature(const_slice_index)]
#![feature(option_zip)]
#![feature(const_float_classify)]
#![feature(int_roundings)]

mod render;
mod utils;
mod input;
mod sound;
use std::{
    cell::{Ref, RefMut},
    fmt::Debug,
    rc::Rc,
    ops::Not,
    cmp::Reverse};
use input::{ParamId, Button};
use render::Renderer;
use sound::{Sound, SoundGen, SoundPlayer, Note, GraphSpec, GraphEvent, SoundEvent};
use utils::{
    HtmlCanvasExt, HtmlDocumentExt,
    VecExt,
    JsResultUtils, OptionExt, JsResult,
    Point,
    MaybeCell, WasmCell, Take,
    document, window, ResultToJsResult, js_error, HitZone, HtmlElementExt, R64};
use web_sys::{
    console::warn_1,
    HtmlCanvasElement,
    ImageData as JsImageData,
    HtmlElement, PointerEvent,
    CanvasRenderingContext2d, MouseEvent};
use wasm_bindgen::{
    JsCast,
    JsValue,
    Clamped as JsClamped,
    closure::Closure as JsClosure};
use js_sys::Function as JsFunction;
use yew::{
    virtual_dom::VList,
    Callback,
    Component,
    Context, Html, html};

/// responsible for playing sounds and frame-by-frame animations
struct Player {
    graph_canvas: HtmlCanvasElement,
    sound_visualiser_canvas: HtmlCanvasElement,
    renderer: Renderer,
    pbar_start: f64,
    js_callback: JsFunction,
    sound_comps: Vec<SoundGen>,
    connections: Vec<Vec<usize>>,
    sound_events: Vec<SoundEvent>,
    sound_player: SoundPlayer,
    starting_note: Note,
    graph_spec: Option<GraphSpec>,
    /// if the outer `Option` is `Some(x)`, the graph will be updated with `x` passed as the
    /// `interaction` argument
    graph_update: Option<Option<GraphEvent>>
}

static GLOBAL_PLAYER: WasmCell<MaybeCell<Player>> = WasmCell::new(MaybeCell::new());

impl Player {
    fn init_global(graph_canvas: HtmlCanvasElement, sound_visualiser_canvas: HtmlCanvasElement)
    -> JsResult<()> {
        fn render(time: f64) {
            _ = js_try!{type = !:
                    let mut handle = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    let Player{ref sound_visualiser_canvas, ref mut renderer, ref graph_canvas,
                        ref mut pbar_start, ref graph_spec, ref mut graph_update, ref starting_note,
                        ref mut sound_comps, ref mut sound_events, ref mut sound_player, ref connections,
                        ref js_callback} = *handle;
                    let time = R64::try_from(time).to_js_result(loc!())?;

                    let err1 = js_try!{
                        let buf = renderer.set_size(sound_visualiser_canvas.width() as usize, sound_visualiser_canvas.height() as usize)
                            .get_in_buffer();
                        sound_player.visualiser().get_byte_frequency_data(buf);
                        sound_visualiser_canvas.get_2d_context(loc!())?.put_image_data(
                            &JsImageData::new_with_u8_clamped_array(
                                JsClamped(renderer.graph().get_out_bytes()), sound_visualiser_canvas.width()).add_loc(loc!())?,
                            0.0, 0.0).add_loc(loc!())?;
                    };

                    let err2 = js_try!{
                        macro_rules! apply_sound_comp {
                            () => {|sound: Sound, _: usize, dst: usize| -> JsResult<Option<Sound>> {
                                let (sound, event) = sound_comps
                                    .get_mut(dst).to_js_result(loc!())?
                                    .transform(sound_player, sound, time).add_loc(loc!())?;
                                if let Some(mut event) = event {
                                    event.element_id = dst;
                                    sound_events.push_sorted_by_key(event, |x| Reverse(*x));
                                }
                                Ok(sound)
                            }};
                        }

                        if *pbar_start == f64::NEG_INFINITY {
                                sound_events.clear();
                                sound_player.end_sounds();
                        } else {
                            let len = sound_events.len();
                            let start = match sound_events.iter().rev().position(|event| time < event.when) {
                                Some(x) => (x > 0).then(|| len - x),
                                None => Some(0)};
                            if let Some(start) = start {
                                let pending: Vec<_> = sound_events.drain(start..).collect();
                                for event in pending {
                                    let mut apply_sound_comp = apply_sound_comp!();
                                    let Some(sound) = apply_sound_comp(Sound::InputNote(*starting_note, None), 0, event.element_id).add_loc(loc!())?
                                        else {continue};
                                    traverse(connections, event.element_id, sound, &mut apply_sound_comp).add_loc(loc!())?;
                                }
                            }
                        }
                        sound_player.poll(time / 1000.0).add_loc(loc!())?;
                    };

                    let err3 = js_try!{
                        if let (Some(spec), Some(update)) = (graph_spec, graph_update.take()) {
                            let (w, h) = (graph_canvas.width(), graph_canvas.height());
                            let ctx = graph_canvas.get_2d_context(loc!())?;
                            ctx.set_fill_style(&"#181818".into());
                            ctx.set_stroke_style(&"#0069E1".into());
                            ctx.set_line_width(3.0);
                            ctx.fill_rect(0.0, 0.0, w as f64, h as f64);
                            ctx.begin_path();
                            unsafe{sound_comps.get_unchecked_mut(spec.element_id)}
                                .graph(w, h, &ctx, update).add_loc(loc!())?;
                            ctx.stroke();
                        }
                        /*if graph.span > 0 {
                            ctx.stroke_with_path(&graph.path);
                            if !pbar_start.is_nan() {
                                if pbar_start.is_infinite() {
                                    *pbar_start = time.copysign(*pbar_start);
                                }
                                if pbar_start.is_sign_negative() && (time + *pbar_start > graph_out_span) {
                                    *pbar_start = f64::NAN;
                                } else {
                                    let x = if pbar_start.is_sign_positive() {
                                        (time - *pbar_start).min(graph.in_span as f64)
                                    } else {
                                        time + *pbar_start + graph.in_span as f64
                                    } / graph.span as f64 * w;
                                    ctx.set_line_dash(dotted_line).add_loc(loc!())?;
                                    ctx.set_line_width(1.0);
                                    ctx.set_line_dash_offset(time / 100.0);
                                    ctx.begin_path();
                                    ctx.move_to(x, 0.0);
                                    ctx.line_to(x, h);
                                    ctx.stroke();
                                    ctx.set_line_dash(solid_line).add_loc(loc!())?;
                                }
                            }
                        }*/
                    };

                    err1.and(err2).and(err3)
                        .and(window().request_animation_frame(js_callback).add_loc(loc!()))
                        .add_loc(loc!())?;
                    return
            }.report_err(loc!());
        }

        let mut sound_player = SoundPlayer::new(r64![120.0]).add_loc(loc!())?;
        let mut sound_comps = vec![SoundGen::new_input(Point{x: 350, y: 500}),
            SoundGen::new_output(Point{x: 550, y: 500}, &mut sound_player)];
        sound_comps[1].set_id(1);
        GLOBAL_PLAYER.set(Self{
            graph_canvas, sound_visualiser_canvas,
            renderer: Renderer::new(),
            graph_spec: None,
            graph_update: None,
            pbar_start: f64::NAN,
            js_callback: JsClosure::<dyn Fn(f64)>::new(render)
                .into_js_value().unchecked_into(),
            sound_events: vec![], sound_comps, starting_note: Note::A2,
            connections: vec![vec![], vec![]],
            sound_player
        }).add_loc(loc!())?;
        Ok(render(0.0))
    }

    fn add_element(&mut self, mut comp: SoundGen) {
        comp.set_id(self.sound_comps.len());
        self.sound_comps.push(comp);
        self.connections.push(vec![]);
    }

    fn remove_element(&mut self, index: usize) -> JsResult<()> {
        self.sound_comps.try_swap_remove(index).to_js_result(loc!())?;
        unsafe{self.sound_comps.get_unchecked_mut(index)}.set_id(index);
        self.connections.try_swap_remove(index).to_js_result(loc!())?;
        let len = self.connections.len();
        for conns in self.connections.iter_mut() {
            conns.retain_mut(|x| {
                if *x == index {return false}
                if *x == len {*x = index}
                true});
        }
        Ok(())
    }

    fn disconnect_element(&mut self, index: usize) -> JsResult<()> {
        self.connections.get_mut(index).to_js_result(loc!())
            .map(Vec::clear)
    }

    fn get_element<'a>(&'a self, index: usize) -> JsResult<&'a SoundGen> {
        self.sound_comps.get(index).to_js_result(loc!())
    }

    fn get_element_mut<'a>(&'a mut self, index: usize) -> JsResult<&'a mut SoundGen> {
        self.sound_comps.get_mut(index).to_js_result(loc!())
    }

    fn get_element_by_point<'a>(&'a self, point: Point) -> Option<&'a SoundGen> {
        self.sound_comps.iter().find(|x| x.contains(point))
    }

    fn get_element_mut_by_point<'a>(&'a mut self, point: Point) -> Option<&'a mut SoundGen> {
        self.sound_comps.iter_mut().find(|x| x.contains(point))
    }

    fn play_sounds(&mut self, note: Note) {
        self.pbar_start = f64::INFINITY;
        self.starting_note = note;
        for element in self.sound_comps.iter_mut() {
            element.reset(&mut self.sound_player);
        }
        self.sound_events.push(SoundEvent{element_id: 0, when: R64::NEG_INFINITY});
    }

    fn end_sounds(&mut self) {
        self.pbar_start = f64::NEG_INFINITY;
    }

    fn connect(&mut self, src_id: usize, dst_id: usize) -> JsResult<bool> {
        let src = self.sound_comps.get(src_id).to_js_result(loc!())?;
        let dst = self.sound_comps.get(dst_id).to_js_result(loc!())?;
        if !src.connectible(dst) {return Ok(false)}
        Ok(unsafe{self.connections.get_unchecked_mut(src_id)}.push_unique(dst_id, |x, y| x == y))
    }

    fn render_editor_plane(&self, ctx: &CanvasRenderingContext2d, width: f64, height: f64, offset: Point) -> JsResult<()> {
        ctx.fill_rect(0.0, 0.0, width, height);
        ctx.begin_path();
        self.sound_comps.iter()
            .for_each(|c| c.draw(&ctx, offset));
        for (src_id, dsts) in self.connections.iter().enumerate() {
            let Some(mut src) = self.sound_comps
                .get(src_id).to_js_result(loc!())?
                .output_point() else {continue};
            src -= offset;
            for dst_id in dsts.iter() {
                let Some(mut dst) = self.sound_comps
                    .get(*dst_id).to_js_result(loc!())?
                    .input_point() else {continue};
                dst -= offset;
                ctx.move_to(src.x as f64, src.y as f64);
                ctx.line_to(dst.x as f64, dst.y as f64);
            }
        }
        Ok(ctx.stroke())
    }

    #[inline] fn set_graph_spec(&mut self, graph_spec: Option<GraphSpec>) {
        self.graph_spec = graph_spec;
        self.graph_update = Some(None);
        self.graph_canvas.set_height((self.graph_canvas.width() as f32 * graph_spec.map_or(0.0, |x| *x.ratio)) as u32)
    }

    #[inline] fn force_graph_update(&mut self) {
        self.graph_update = Some(None);
    }

    /// `f` will only be evaluated if there's an element being graphed and it's marked as
    /// interactive
    #[inline] fn emit_graph_interaction(&mut self, f: impl FnOnce() -> Option<GraphEvent>) {
        if self.graph_spec.map_or(false, |x| x.interactive) {
            self.graph_update = Some(f());
        }
    }
}

#[derive(Default)]
enum Focus {
    #[default] None,
    PlaneHover(usize),
    Plane,
    Graph,
    Element(usize)
}

pub struct Main {
    selected_comp: Option<usize>,
    error_count: usize,
    plane_offset: Point,
    graph_canvas: HtmlCanvasElement,
    sound_visualiser_canvas: HtmlCanvasElement,
    editor_plane_canvas: HtmlCanvasElement,
    help_msg_bar: HtmlElement,
    focus: Focus
}

fn traverse<S: Clone + Debug>(conns: &[Vec<usize>], src_id: usize, state: S, f: &mut impl FnMut(S, usize, usize) -> JsResult<Option<S>>)
-> JsResult<()> {
    let dsts = conns.get(src_id).to_js_result(loc!())?;
    for dst_id in dsts.iter() {
        let Some(new_state) = f(state.clone(), src_id, *dst_id).add_loc(loc!())?
            else {return Ok(())};
        traverse(conns, *dst_id, new_state, f).add_loc(loc!())?;
    }
    Ok(())
}

#[derive(Debug)]
pub enum MainCmd {
    Hover(PointerEvent),
    Focus(PointerEvent),
    Unfocus(PointerEvent),
    FocusGraph(PointerEvent),
    UnfocusGraph(PointerEvent),
    LeavePlane,
    SetDesc(String),
    RemoveDesc,
    SetParam(ParamId, R64),
    TryConnect(usize, Point),
    Select(Option<usize>),
    ReportError(JsValue)
}

static mut MAINCMD_SENDER: Option<Callback<MainCmd>> = None;
impl MainCmd {
    #[inline] pub fn send(self) {
        unsafe{MAINCMD_SENDER.as_ref().unwrap_unchecked()}.emit(self)
    }
}

impl Main {
    const DEF_HELP_MSG: &'static str = "Hover over an element to get help";

    fn set_desc(&self, desc: &str) {
        self.help_msg_bar.set_inner_text(&format!("{:1$}", desc, Self::DEF_HELP_MSG.len()))
    }

    fn remove_desc(&self) {
        self.help_msg_bar.set_inner_text(Self::DEF_HELP_MSG)
    }

    fn new_graph_event(&self, init: &MouseEvent) -> Option<GraphEvent> {
        let canvas_rect = self.graph_canvas.client_rect();
        let point = Point{x: init.offset_x(), y: init.offset_y()};
        canvas_rect.contains(point).then(|| 
            GraphEvent{shift: init.shift_key(), alt: init.alt_key(),
                point: point.normalise(canvas_rect, self.graph_canvas.rect())})
    }
}

impl Component for Main {
    type Message = MainCmd;
    type Properties = ();

    fn create(ctx: &Context<Self>) -> Self {
        *unsafe{&mut MAINCMD_SENDER} = Some(ctx.link().callback(|msg| msg));

        let help_msg_bar: HtmlElement = document().create_element("div").unwrap_throw(loc!())
            .unchecked_into();
        help_msg_bar.set_id("help-msg");
        help_msg_bar.set_inner_text(Self::DEF_HELP_MSG);
        help_msg_bar.set_class_name("light-bg");

        Self {help_msg_bar,
            selected_comp: None, focus: Focus::None,
            plane_offset: Point::ZERO,
            graph_canvas: JsValue::UNDEFINED.unchecked_into(),
            sound_visualiser_canvas: JsValue::UNDEFINED.unchecked_into(),
            editor_plane_canvas: JsValue::UNDEFINED.unchecked_into(),
            error_count: 0}
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        let on_new_error = |this: &mut Self, err: JsValue| -> bool {
            this.error_count += 1;
            warn_1(&err);
            true
        };

        let err = js_try!{type = !:
            let mut needs_html_rerender = false;
            let mut player: Option<RefMut<Player>> = None;
            match msg {
                MainCmd::Hover(e) => {
                    let player = player.insert(GLOBAL_PLAYER.get_mut().add_loc(loc!())?);
                    match self.focus {
                        Focus::PlaneHover(id) => _ = player.get_element(id).add_loc(loc!())?
                            .contains(Point{x: e.x(), y: e.y()} + self.plane_offset).not()
                            .then(|| {
                                self.focus = Focus::None;
                                self.remove_desc()}),

                        Focus::Plane => self.plane_offset -= Point{x: e.movement_x(), y: 0},

                        Focus::Graph => player.emit_graph_interaction(|| self.new_graph_event(&e)),

                        Focus::Element(id) => _ = player.get_element_mut(id).add_loc(loc!())?
                            .handle_movement(Point{x: e.x(), y: e.y()} + self.plane_offset, false)
                            .map(|msg| ctx.link().send_message(msg)),

                        Focus::None => _ = player
                            .get_element_by_point(Point{x: e.x(), y: e.y()} + self.plane_offset)
                            .map(|comp| {
                                self.focus = Focus::PlaneHover(comp.id());
                                self.set_desc(comp.name())})
                    }
                }

                MainCmd::Focus(e) => {
                    self.editor_plane_canvas.set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    let point = Point{x: e.x(), y: e.y()} + self.plane_offset;
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    if let Some(comp) = player.get_element_mut_by_point(point) {
                        self.focus = Focus::Element(comp.id());
                        comp.handle_movement(point, false)
                            .map(|msg| ctx.link().send_message(msg));
                    } else {
                        self.focus = Focus::Plane;
                        self.set_desc("Hovering the plane");
                    }
                }

                MainCmd::Unfocus(e) => {
                    self.editor_plane_canvas.release_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    if let Focus::Element(id) = self.focus.take() {
                        let point = Point{x: e.x(), y: e.y()} + self.plane_offset;
                        let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                        let comp = player.get_element_mut(id).add_loc(loc!())?;
                        self.set_desc(comp.name());
                        if let Some(msg) = comp.handle_movement(point, true) {
                            ctx.link().send_message(msg);
                        }
                    } else {
                        ctx.link().send_message(MainCmd::Select(None));
                        self.remove_desc();
                    }
                }

                MainCmd::FocusGraph(e) => {
                    self.focus = Focus::Graph;
                    self.graph_canvas.set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    GLOBAL_PLAYER.get_mut().add_loc(loc!())?.emit_graph_interaction(|| self.new_graph_event(&e))
                }

                MainCmd::UnfocusGraph(e) => {
                    self.focus = Focus::None;
                    self.graph_canvas.release_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    GLOBAL_PLAYER.get_mut().add_loc(loc!())?.force_graph_update();
                }

                MainCmd::LeavePlane => {
                    self.focus = Focus::None;
                    self.remove_desc();
                }

                MainCmd::SetDesc(value) => self.set_desc(&value),

                MainCmd::RemoveDesc => self.remove_desc(),

                MainCmd::SetParam(id, value) => {
                    if let Some(element_id) = id.element_id() {
                        let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                        let element = player.get_element_mut(element_id).add_loc(loc!())?;
                        needs_html_rerender = element.set_param(id, value).add_loc(loc!())?;
                        player.force_graph_update();
                    } else {
                        let player = player.insert(GLOBAL_PLAYER.get_mut().add_loc(loc!())?);
                        match id {
                            ParamId::Play(note) => if value.is_sign_positive() {
                                player.play_sounds(note)
                            } else {
                                player.end_sounds()
                            }
                            ParamId::Disconnect(id) =>
                                player.disconnect_element(id).add_loc(loc!())?,
                            ParamId::Remove(id) => {
                                player.remove_element(id).add_loc(loc!())?;
                                self.selected_comp = None;
                                needs_html_rerender = true;
                            }
                            id => js_error(format!("invalid parameter ID: `{:?}`", id), loc!())?
                        }
                    }
                }

                MainCmd::TryConnect(src, dst_pos) => {
                    let player = player.insert(GLOBAL_PLAYER.get_mut().add_loc(loc!())?);
                    if let Some(dst) = player.get_element_by_point(dst_pos).map(|x| x.id()) {
                        if player.connect(src, dst).add_loc(loc!())? {
                            ctx.link().send_message(MainCmd::Select(Some(dst)));
                        }
                    } else {
                        self.remove_desc()
                    }
                }

                MainCmd::Select(id) => {
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    self.selected_comp = id.filter(|id| Some(*id) != self.selected_comp);
                    if let Some(id) = self.selected_comp {
                        let graph_spec = player.get_element_mut(id).add_loc(loc!())?.graph_spec();
                        player.set_graph_spec(graph_spec);
                    } else {
                        player.set_graph_spec(None)}
                    needs_html_rerender = true;
                }

                MainCmd::ReportError(err) => return on_new_error(self, err)
            };

            if let Some(player) = player {
                player.render_editor_plane(
                    &self.editor_plane_canvas.get_2d_context(loc!())?,
                    self.editor_plane_canvas.width() as f64,
                    self.editor_plane_canvas.height() as f64,
                    self.plane_offset).add_loc(loc!())?;
            }
            return needs_html_rerender
        };
        on_new_error(self, err.into_err())
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let comp = self.selected_comp.map(|id|
            Ref::filter_map(GLOBAL_PLAYER.get().expect_throw("building the app view"), |x| x.get_element(id).ok())
                .map_err(|_| format!("element #{} not found", id).into()).unwrap_throw(loc!()));

        fn piano_roll() -> Html {
            let mut list = VList::new();
            list.reserve_exact(Note::ALL.len() + 1);
            const N_DIATONIC_NOTES: f64 = Note::N_OCTAVES as f64 * 7.0;
            let width: Rc<str> = format!("{:.2}%", 100.0 / N_DIATONIC_NOTES).into();

            for note in Note::ALL.iter().filter(|x| !x.is_sharp()) {
                let x = note.diatonic_index() as f64 / (N_DIATONIC_NOTES / 100.0);
                list.add_child(html!{
                    <Button svg={true} class="piano-note"
                     id={ParamId::Play(*note)}
                     desc={format!("Play the {} note", note.name())}>
                        <rect width={width.clone()} height="100%" 
                         x={format!("{:.2}%", x)} y="0"/>
                    </Button>});
            }

            for note in Note::ALL.iter().filter(|x| x.is_sharp()) {
                let x = (note.diatonic_index() as f64 + 0.5) / (N_DIATONIC_NOTES / 100.0);
                list.add_child(html!{
                    <Button svg={true} class="piano-note sharp-note"
                     id={ParamId::Play(*note)}
                     desc={format!("Play the {} note", note.name())}>
                        <rect width={width.clone()} height="50%" 
                         x={format!("{:.2}%", x)} y="0"/>
                    </Button>});
            }

            list.add_child(html!{<line class="piano-note" x1="100%" x2="100%" y2="100%" transform="translate(-4.5, 0)"/>});
            Html::VList(list)
        }

        return html! {<>
            <canvas width="100%" height="100%" id="plane"
            onpointerdown={ctx.link().callback(MainCmd::Focus)}
            onpointerup={ctx.link().callback(MainCmd::Unfocus)}
            onpointermove={ctx.link().callback(MainCmd::Hover)}
            onpointerleave={ctx.link().callback(|_| MainCmd::LeavePlane)}/>
            <div id="ctrl-panel" class="dark-bg">
                {Html::VRef(self.help_msg_bar.clone().into())}
                {comp.as_ref().map(|x| x.params())}
                <canvas id="graph" class="blue-border" height=0
                onpointerdown={ctx.link().callback(MainCmd::FocusGraph)}
                onpointerup={ctx.link().callback(MainCmd::UnfocusGraph)}
                onpointermove={ctx.link().callback(MainCmd::Hover)}/>
                if let Some(comp_id) = self.selected_comp {
                    <div id="general-ctrl" class="dark-bg">
                        <Button
                        id={ParamId::Disconnect(comp_id)}
                        desc={"Disconnect component"}>
                            <svg viewBox="0 0 100 100">
                                <polygon points="10,40 10,60 40,60 30,40"/>
                                <polygon points="30,20 60,80 70,80 40,20"/>
                                <polygon points="50,40 80,40 80,60 60,60"/>
                            </svg>
                        </Button>
                        <Button
                        id={ParamId::Remove(comp_id)}
                        desc={"Remove component"}>
                            <svg viewBox="0 0 100 100">
                                <polygon points="27,35 35,27 50,42 65,27 73,35 58,50 73,65 65,73 50,58 35,73 27,65 42,50"/>
                            </svg>
                        </Button>
                    </div>
                }
            </div>
            <div id="io-panel">
                <svg id="piano" viewBox={format!("-3 0 {} 100", Note::ALL.len() * 20 - 3)}>
                    {piano_roll()}
                </svg>
                <canvas id="sound-visualiser" class="blue-border"/>
            </div>
            if self.error_count > 0 {
                <div id="error-count">{format!("Errors: {}", self.error_count)}</div>
            }
        </>}
    }

    fn rendered(&mut self, _: &Context<Self>, first_render: bool) {
        _ = js_try!{type = !:
            if first_render {
                self.graph_canvas = document().element_dyn_into("graph", loc!())?;
                self.sound_visualiser_canvas = document().element_dyn_into("sound-visualiser", loc!())?;
                self.sound_visualiser_canvas.sync();
                self.help_msg_bar = document().element_dyn_into("help-msg", loc!())?;

                self.editor_plane_canvas = document().element_dyn_into("plane", loc!())?;
                let body = document().body().to_js_result(loc!())?;
                let (w, h) = (body.client_width(), body.client_height());
                self.editor_plane_canvas.set_width(w as u32);
                self.editor_plane_canvas.set_height(h as u32);

                let ctx = self.editor_plane_canvas.get_2d_context(loc!())?;
                ctx.set_stroke_style(&"#0069E1".into());
                ctx.set_fill_style(&"#232328".into());
                ctx.set_line_width(3.0);
                Player::init_global(self.graph_canvas.clone(),
                    self.sound_visualiser_canvas.clone()).add_loc(loc!())?;
                let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                player.add_element(SoundGen::new_wave(Point{x: 350, y: 441}));
                player.add_element(SoundGen::new_envelope(Point{x: 550, y: 441}));
                player.add_element(SoundGen::new_pattern(Point{x: 450, y: 278}));
                player.render_editor_plane(&ctx, w as f64, h as f64, Point::ZERO).add_loc(loc!())?;
            }
            return
        }.expect_throw("rendering the main element");
    }
}

fn main() {
    yew::Renderer::<Main>::new().render();
}
