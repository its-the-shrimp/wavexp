#![feature(try_blocks)]
#![feature(never_type)]
#![feature(unwrap_infallible)]
#![feature(const_float_classify)]
#![feature(const_slice_index)]

mod render;
mod utils;
mod input;
mod sound;
use std::cell::{Ref, RefMut};
use std::fmt::Debug;
use input::{ParamId, Button};
use render::Renderer;
use sound::{Sound, SoundGen, SoundPlayer, Graph};
use utils::{
    HtmlCanvasExt, HtmlDocumentExt,
    SliceExt, VecExt,
    JsResultUtils, OptionToJsResult, JsResult,
    Point,
    MaybeCell, WasmCell,
    document, window, ResultToJsResult, js_error};
use web_sys::CanvasRenderingContext2d;
use web_sys::{
    console::warn_1,
    Path2d as JsPath2d,
    HtmlCanvasElement,
    ImageData as JsImageData,
    HtmlElement, PointerEvent};
use wasm_bindgen::{
    JsValue,
    Clamped as JsClamped,
    closure::Closure as JsClosure};
use wasm_bindgen::JsCast;
use js_sys::{
    Array as JsArray,
    Function as JsFunction};
use yew::{
    Callback,
    Component,
    Context, Html, html};

/// responsible for playing sounds and frame-by-frame animations
pub struct Player {
    graph_canvas: HtmlCanvasElement,
    sound_visualiser_canvas: HtmlCanvasElement,
    renderer: Renderer,
    solid_line: JsValue,
    dotted_line: JsValue,
    graph: Graph,
    pbar_start: f64,
    js_callback: JsFunction,
    sound_comps: Vec<SoundGen>,
    connections: Vec<Vec<usize>>,
    sound_comp_events: Vec<(usize, f64)>,
    sound_player: SoundPlayer,
    ending_all: bool
}

static GLOBAL_PLAYER: WasmCell<MaybeCell<Player>> = WasmCell::new(MaybeCell::new());

impl Player {
    fn init_global(graph_canvas: HtmlCanvasElement, sound_visualiser_canvas: HtmlCanvasElement)
    -> JsResult<()> {
        fn render(time: f64) {
            _ = js_try!{type = !:
                    let mut handle = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    let Player{ref sound_visualiser_canvas, ref mut renderer,
                        ref graph_canvas, ref solid_line, ref dotted_line,
                        ref mut pbar_start, ref graph,
                        ref mut sound_comps, ref mut sound_comp_events, ref mut sound_player, ref connections, ref mut ending_all,
                        ref js_callback} = *handle;
                    let graph_out_span = (graph.span - graph.in_span) as f64;

                    let err1 = js_try!{
                        let buf = renderer.set_size(sound_visualiser_canvas.width() as usize, sound_visualiser_canvas.height() as usize)
                            .get_in_buffer();
                        sound_player.output().get_byte_frequency_data(buf);
                        sound_visualiser_canvas.get_2d_context().add_loc(loc!())?.put_image_data(
                            &JsImageData::new_with_u8_clamped_array(
                                JsClamped(renderer.graph().get_out_bytes()), sound_visualiser_canvas.width()).add_loc(loc!())?,
                            0.0, 0.0).add_loc(loc!())?;
                    };

                    let err2 = js_try!{
                        let (w, h, ctx) = (graph_canvas.width().into(), graph_canvas.height().into(), graph_canvas.get_2d_context().add_loc(loc!())?);
                        ctx.set_fill_style(&"#181818".into());
                        ctx.fill_rect(0.0, 0.0, w, h);
                        if graph.span > 0 {
                            ctx.set_line_width(3.0);
                            ctx.set_stroke_style(&"#0069E1".into());
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
                        }
                    };

                    let err3 = js_try!{
                        macro_rules! apply_sound_comp {
                            () => {|sound: Sound, _: usize, dst: usize| -> JsResult<Sound> {
                                let (sound, after) = sound_comps.get_mut_or_js_error(dst, "sound element #", " not found").add_loc(loc!())?
                                    .transform(sound_player, sound).add_loc(loc!())?;
                                if let Some(after) = after {
                                    sound_comp_events.push_sorted((dst, after as f64 / 1000.0 + time), |x, y| x.1.total_cmp(&y.1).reverse());
                                }
                                Ok(sound)
                            }};
                        }

                        if *ending_all {
                            *ending_all = false;
                            sound_comp_events.clear();
                            traverse(connections, 0, Sound::End, &mut apply_sound_comp!()).add_loc(loc!())?;
                        } else {
                            let len = sound_comp_events.len();
                            let start = match sound_comp_events.iter().rev().position(|event| time < event.1) {
                                Some(x) => (x > 0).then(|| len - x),
                                None => Some(0)};
                            if let Some(start) = start {
                                let pending: Vec<_> = sound_comp_events.drain(start..).collect();
                                for (index, _) in pending {
                                    let mut apply_sound_comp = apply_sound_comp!();
                                    let sound = apply_sound_comp(Sound::InputFreq(0.0), 0, index).add_loc(loc!())?;
                                    traverse(connections, index, sound, &mut apply_sound_comp).add_loc(loc!())?;
                                }
                            }
                        }
                        sound_player.poll(time).add_loc(loc!())?;
                    };

                    err1.and(err2).and(err3)
                        .and(window().request_animation_frame(js_callback).add_loc(loc!()))
                        .add_loc(loc!())?;
                    return
            }.report_err();
        }

        let mut sound_comps = vec![SoundGen::new_input(Point{x: 350, y: 500}),
            SoundGen::new_output(Point{x: 550, y: 500})];
        sound_comps[1].set_id(1);
        GLOBAL_PLAYER.set(Self{
            graph_canvas, sound_visualiser_canvas,
            renderer: Renderer::new(),
            solid_line: JsArray::new().into(),
            dotted_line: JsArray::of2(&(10.0).into(), &(10.0).into()).into(),
            graph: Graph{path: JsPath2d::new().add_loc(loc!())?, span: 0, in_span: 0},
            pbar_start: f64::NAN,
            js_callback: JsClosure::<dyn Fn(f64)>::new(render)
                .into_js_value().unchecked_into(),
            sound_comp_events: vec![], ending_all: false, sound_comps,
            connections: vec![vec![], vec![]], sound_player: SoundPlayer::new().add_loc(loc!())?
        }).add_loc(loc!())?;
        Ok(render(0.0))
    }

    #[inline] fn set_graph(&mut self, graph: Graph) {self.graph = graph}

    #[inline] fn clear_graph(&mut self) {self.graph.span = 0}

    fn add_element(&mut self, mut comp: SoundGen) {
        comp.set_id(self.sound_comps.len());
        self.sound_comps.push(comp);
        self.connections.push(vec![]);
    }

    fn remove_element(&mut self, index: usize) -> JsResult<()> {
        self.sound_comps.try_swap_remove(index).to_js_result().add_loc(loc!())?;
        unsafe{self.sound_comps.get_unchecked_mut(index)}.set_id(index);
        self.connections.try_swap_remove(index).to_js_result().add_loc(loc!())?;
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
        self.connections
            .get_mut_or_js_error(index, "sound element #", " not found").add_loc(loc!())
            .map(Vec::clear)
    }

    fn get_element<'a>(&'a self, index: usize) -> JsResult<&'a SoundGen> {
        self.sound_comps
            .get_or_js_error(index, "sound element #", " not found").add_loc(loc!())
    }

    fn get_element_mut<'a>(&'a mut self, index: usize) -> JsResult<&'a mut SoundGen> {
        self.sound_comps
            .get_mut_or_js_error(index, "sound element #", " not found").add_loc(loc!())
    }

    fn get_element_by_point<'a>(&'a self, point: Point) -> Option<&'a SoundGen> {
        self.sound_comps.iter().find(|x| x.contains(point))
    }

    fn get_element_mut_by_point<'a>(&'a mut self, point: Point) -> Option<&'a mut SoundGen> {
        self.sound_comps.iter_mut().find(|x| x.contains(point))
    }

    fn play_sounds(&mut self) {
        self.pbar_start = f64::INFINITY;
        self.sound_comp_events.push((0, f64::NEG_INFINITY));
    }

    fn end_sounds(&mut self) {
        self.pbar_start = f64::NEG_INFINITY;
        self.ending_all = true;
    }

    fn connect(&mut self, src_id: usize, dst_id: usize) -> JsResult<bool> {
        let src = self.sound_comps.get_or_js_error(src_id, "sound element #", " not found").add_loc(loc!())?;
        let dst = self.sound_comps.get_or_js_error(dst_id, "sound element #", " not found").add_loc(loc!())?;
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
                .get_or_js_error(src_id, "sound element #", " not found").add_loc(loc!())?
                .output_point() else {continue};
            src -= offset;
            for dst_id in dsts.iter() {
                let Some(mut dst) = self.sound_comps
                    .get_or_js_error(*dst_id, "sound element #", " not found").add_loc(loc!())?
                    .input_point() else {continue};
                dst -= offset;
                ctx.move_to(src.x as f64, src.y as f64);
                ctx.line_to(dst.x as f64, dst.y as f64);
            }
        }
        Ok(ctx.stroke())
    }
}

pub struct Main {
    selected_comp: Option<usize>,
    focused_comp: Option<usize>,
    hovered_comp: Option<usize>,
    error_count: usize,
    plane_moving: bool,
    plane_offset: Point,
    graph_canvas: HtmlCanvasElement,
    sound_visualiser_canvas: HtmlCanvasElement,
    editor_plane_canvas: HtmlCanvasElement,
    help_msg_bar: HtmlElement,
}

fn traverse<S: Clone + Debug>(conns: &[Vec<usize>], src_id: usize, state: S, f: &mut impl FnMut(S, usize, usize) -> JsResult<S>)
-> JsResult<()> {
    let dsts = conns.get_or_js_error(src_id, "element #", " not found").add_loc(loc!())?;
    for dst_id in dsts.iter() {
        let new_state = f(state.clone(), src_id, *dst_id).add_loc(loc!())?;
        traverse(conns, *dst_id, new_state, f).add_loc(loc!())?;
    }
    Ok(())
}

#[derive(Debug)]
pub enum MainCmd {
    Drag(PointerEvent),
    Focus(PointerEvent),
    Unfocus(PointerEvent),
    LeavePlane,
    SetDesc(String),
    RemoveDesc,
    SetParam(ParamId, f64),
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
}

impl Component for Main {
    type Message = MainCmd;
    type Properties = ();

    fn create(ctx: &Context<Self>) -> Self {
        *unsafe{&mut MAINCMD_SENDER} = Some(ctx.link().callback(|msg| msg));

        let help_msg_bar: HtmlElement = document().create_element("div").add_loc(loc!())
            .unwrap_throw().unchecked_into();
        help_msg_bar.set_id("help-msg");
        help_msg_bar.set_inner_text(Self::DEF_HELP_MSG);

        Self {help_msg_bar,
            selected_comp: None, focused_comp: None, hovered_comp: None,
            plane_offset: Point::ZERO, plane_moving: false,
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
                MainCmd::Drag(e) => {
                    let player = player.insert(GLOBAL_PLAYER.get_mut().add_loc(loc!())?);
                    if self.plane_moving {
                        self.plane_offset -= Point{x: e.movement_x(), y: 0};
                    } else {
                        let point = Point{x: e.x(), y: e.y()} + self.plane_offset;
                        if let Some(id) = self.focused_comp {
                            if let Some(msg) = player.get_element_mut(id).add_loc(loc!())?.handle_movement(point, false) {
                                needs_html_rerender = self.update(ctx, msg)
                            }
                        } else {
                            if let Some(id) = self.hovered_comp {
                                if !player.get_element(id).add_loc(loc!())?.contains(point) {
                                    self.hovered_comp = None;
                                    self.remove_desc();
                                }
                            }
                            if self.hovered_comp.is_none() {
                                if let Some(comp) = player.get_element_by_point(point) {
                                    self.hovered_comp = Some(comp.id());
                                    self.set_desc(comp.name());
                                }
                            }
                        }
                    }
                }

                MainCmd::Focus(e) => {
                    self.editor_plane_canvas.set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    let point = Point{x: e.x(), y: e.y()} + self.plane_offset;
                    let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                    if let Some(comp) = player.get_element_mut_by_point(point) {
                        self.focused_comp = Some(comp.id());
                        if let Some(msg) = comp.handle_movement(point, false) {
                            ctx.link().send_message(msg);
                        }
                    } else {
                        self.plane_moving = true;
                        self.set_desc("Dragging the plane");
                    }
                }

                MainCmd::Unfocus(e) => {
                    self.editor_plane_canvas.release_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                    if let Some(id) = self.focused_comp.take() {
                        let point = Point{x: e.x(), y: e.y()} + self.plane_offset;
                        let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                        let comp = player.get_element_mut(id).add_loc(loc!())?;
                        self.set_desc(comp.name());
                        if let Some(msg) = comp.handle_movement(point, true) {
                            ctx.link().send_message(msg);
                        }
                    } else {
                        ctx.link().send_message(MainCmd::Select(None));
                        self.plane_moving = false;
                        self.remove_desc();
                    }
                }

                MainCmd::LeavePlane => {
                    self.hovered_comp = None;
                    self.remove_desc();
                }

                MainCmd::SetDesc(value) => self.set_desc(&value),

                MainCmd::RemoveDesc => self.remove_desc(),

                MainCmd::SetParam(id, value) => {
                    if let Some(element_id) = id.element_id() {
                        let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                        let element = player.get_element_mut(element_id).add_loc(loc!())?;
                        needs_html_rerender = element.set_param(id, value).add_loc(loc!())?;
                        let graph = element.graph(self.graph_canvas.width() as f64, self.graph_canvas.height() as f64).add_loc(loc!())?;
                        player.set_graph(graph);
                    } else {
                        let player = player.insert(GLOBAL_PLAYER.get_mut().add_loc(loc!())?);
                        match id {
                            ParamId::Play => if value.is_sign_positive() {
                                player.play_sounds()
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
                        let graph = player.get_element(id).add_loc(loc!())?
                            .graph(self.graph_canvas.width().into(), self.graph_canvas.height().into()).add_loc(loc!())?;
                        player.set_graph(graph);
                    } else {
                        player.clear_graph()}
                    needs_html_rerender = true;
                }

                MainCmd::ReportError(err) => return on_new_error(self, err)
            };

            if let Some(player) = player {
                player.render_editor_plane(
                    &self.editor_plane_canvas.get_2d_context().add_loc(loc!())?,
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
                .ok().to_js_result_with(|| format!("sound element #{} not found", id)).add_loc(loc!())
                .expect_throw("building the app view"));

        return html! {<>
            <canvas width="100%" height="100%" id="plane"
            onpointerdown={ctx.link().callback(MainCmd::Focus)}
            onpointerup={ctx.link().callback(MainCmd::Unfocus)}
            onpointermove={ctx.link().callback(MainCmd::Drag)}
            onpointerleave={ctx.link().callback(|_| MainCmd::LeavePlane)}/>
            <div id="ctrl-panel">
                {Html::VRef(self.help_msg_bar.clone().into())}
                {comp.as_ref().map(|x| x.params())}
                <canvas id="graph" class="visual"
                hidden={comp.map(|x| !x.graphable()).unwrap_or(true)}/>
                if let Some(comp_id) = self.selected_comp {
                    <div id="general-ctrl">
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
            <div id="visuals">
                <canvas id="sound-visualiser" class="visual"/>
                <input::Button
                id={ParamId::Play}
                desc={"Play"}>
                    <svg viewBox="0 0 100 100" height="100%">
                        <polygon points="25,25 75,50 25,75"/>
                    </svg>
                </input::Button>
            </div>
            if self.error_count > 0 {
                <div id="error-count">{format!("Errors: {}", self.error_count)}</div>
            }
        </>}
    }

    fn rendered(&mut self, _: &Context<Self>, first_render: bool) {
        _ = js_try!{type = !:
            return if first_render {
                self.graph_canvas = document().element_dyn_into("graph").add_loc(loc!())?;
                self.graph_canvas.set_hidden(false);
                self.graph_canvas.sync();
                self.graph_canvas.set_hidden(true);
                self.sound_visualiser_canvas = document().element_dyn_into("sound-visualiser").add_loc(loc!())?;
                self.sound_visualiser_canvas.sync();
                self.help_msg_bar = document().element_dyn_into("help-msg").add_loc(loc!())?;

                self.editor_plane_canvas = document().element_dyn_into("plane").add_loc(loc!())?;
                let body = document().body().to_js_result("<body> not found").add_loc(loc!())?;
                let (w, h) = (body.client_width(), body.client_height());
                self.editor_plane_canvas.set_width(w as u32);
                self.editor_plane_canvas.set_height(h as u32);

                let ctx = self.editor_plane_canvas.get_2d_context().add_loc(loc!())?;
                ctx.set_stroke_style(&"#0069E1".into());
                ctx.set_fill_style(&"#232328".into());
                ctx.set_line_width(3.0);
                Player::init_global(self.graph_canvas.clone(),
                    self.sound_visualiser_canvas.clone()).add_loc(loc!())?;
                let mut player = GLOBAL_PLAYER.get_mut().add_loc(loc!())?;
                player.add_element(SoundGen::new_wave(Point{x: 350, y: 441}));
                player.add_element(SoundGen::new_envelope(Point{x: 550, y: 441}));
                player.render_editor_plane(&ctx, w as f64, h as f64, Point::ZERO).add_loc(loc!())?;
            }
        }.expect_throw("rendering the main element");
    }
}

fn main() {
    yew::Renderer::<Main>::new().render();
}
