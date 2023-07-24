use std::{
    iter::Iterator,
    slice::{from_raw_parts, from_mut},
    mem::{replace, take},
    ops::{Range, Deref, Not, DerefMut},
    fmt::Debug,
    rc::Rc,
    cmp::{min, Ordering},
    cell::{LazyCell, Cell}, any::Any, borrow::Cow};
use web_sys::{
    HtmlCanvasElement,
    AnalyserNode,
    ImageData,
    HtmlElement,
    Path2d,
    AudioContext, 
    SvgElement, 
    Element, MouseEvent};
use wasm_bindgen::{Clamped, JsValue, JsCast};
use yew::{TargetCast, NodeRef, function_component, Callback, Properties, scheduler::Shared, html, Html};
use crate::{
    utils::{SliceExt, Point,
        JsResult, HtmlCanvasExt, JsResultUtils, OptionExt,
        HtmlElementExt, 
        Pipe, BoolExt, RangeExt, VecExt, R64, ArrayExt, ArrayFrom, IntoArray, SliceRef, ResultToJsResult, SliceMove, RoundTo, FlippedArray, default, report_err, WasmCell, Alias},
    sound::{FromBeats, Secs},
    global::{AppEvent, AppContext, AppAction},
    input::{Buttons, Cursor},
    loc,
    r64,
    js_assert,
    eval_once,
    js_array
};

pub struct EveryNth<'a, T> {
    iter: &'a [T],
    n: usize,
    state: usize,
    off: usize
}

impl<'a, T> Iterator for EveryNth<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
		if let res @Some(_) = self.iter.get(self.state) {
            self.state += self.n;
            res
        } else {
            self.off += 1;
            if self.off == self.n {
                None
            } else {
                self.state = self.off + self.n;
                self.iter.get(self.state - self.n)
            }
        }
    }
}

pub struct EveryNthMut<'a, T> {
    iter: &'a mut [T],
    n: usize,
    state: usize,
    off: usize
}

impl<'a, T> Iterator for EveryNthMut<'a, T> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
		if let Some(res) = self.iter.get_mut(self.state) {
            self.state += self.n;
            Some(unsafe{(res as *mut T).as_mut().unwrap_unchecked()})
        } else {
            self.off += 1;
            if self.off == self.n {
                None
            } else {
                self.state = self.off + self.n;
                self.iter.get_mut(self.state - self.n)
                    .map(|x| unsafe{(x as *mut T).as_mut().unwrap_unchecked()})
            }
        }
    }
}

pub trait ToEveryNth<T> {
    fn every_nth(&self, n: usize) -> EveryNth<'_, T>;
    fn every_nth_mut(&mut self, n: usize) -> EveryNthMut<'_, T>;
}

impl<T> ToEveryNth<T> for [T] {
    #[inline] fn every_nth(&self, n: usize) -> EveryNth<'_, T> {
        EveryNth {iter: self, n, state: 0, off: 0}
    }
    #[inline] fn every_nth_mut(&mut self, n: usize) -> EveryNthMut<'_, T> {
        EveryNthMut {iter: self, n, state: 0, off: 0}
    }
}

#[test]
fn test_every_nth_mut() {
    let mut data = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    let transposed: Vec<u8>     = data.every_nth(3).copied().collect();
    let transposed_mut: Vec<u8> = data.every_nth_mut(3).map(|x| *x).collect();
    assert_eq!(transposed,     [0, 3, 6, 9, 1, 4, 7, 10, 2, 5, 8]);
    assert_eq!(transposed_mut, [0, 3, 6, 9, 1, 4, 7, 10, 2, 5, 8]);
}


#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Rgba {
	pub r: u8,
	pub g: u8,
	pub b: u8,
	pub a: u8
}

impl From<u32> for Rgba {
	fn from(val: u32) -> Self {
		Self {
			r: (val >> 24) as u8,
			g: ((val >> 16) & 0xFF) as u8,
			b: ((val >>  8) & 0xFF) as u8,
			a: (val & 0xFF) as u8}
	}
}

impl From<Rgba> for u32 {
	fn from(val: Rgba) -> Self {
		val.a as u32
		| (val.b as u32) << 8
		| (val.g as u32) << 16
		| (val.r as u32) << 24
	}
}

impl std::fmt::Display for Rgba {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "#{:02X}{:02X}{:02X}{:02X}", self.r, self.g, self.b, self.a)
	}
}

fn interp<const N: usize>(colours: &[Rgba; N], index: u8) -> Rgba {
	let index = (index as usize * (N - 1)) as f32 / 255.0;
	let lower = colours.get_saturating(index.floor() as usize);
	let upper = colours.get_saturating(index.ceil() as usize);
	let weight = (index / (N - 1) as f32).fract();
	let weight_recip = 1.0 - weight;
	Rgba {
		r: (lower.r as f32 * weight_recip + upper.r as f32 * weight) as u8,
		g: (lower.g as f32 * weight_recip + upper.g as f32 * weight) as u8,
		b: (lower.b as f32 * weight_recip + upper.b as f32 * weight) as u8,
		a: (lower.a as f32 * weight_recip + upper.a as f32 * weight) as u8}
}

pub struct SoundVisualiser {
    input: Rc<AnalyserNode>,
	out_data: Vec<Rgba>,
	in_data: Vec<u8>,
    gradient: Vec<Rgba>,
    canvas: NodeRef,
    width: u32, height: u32
}

impl SoundVisualiser {
	pub const FG: Rgba = Rgba{r:0x00, g:0x69, b:0xE1, a:0xFF};
	pub const BG: Rgba = Rgba{r:0x18, g:0x18, b:0x18, a:0xFF};
	pub fn new(ctx: &AudioContext) -> JsResult<Self> {
		Ok(Self{input: Rc::new(ctx.create_analyser().add_loc(loc!())?),
            out_data: vec![], in_data: vec![],
            gradient: (0 ..= u8::MAX)
                .map(|i| interp(&[Self::BG, Self::FG], i))
                .collect(),
			width: 0, height: 0, canvas: NodeRef::default()})
	}

    #[inline] pub fn canvas(&self) -> &NodeRef {&self.canvas}

    #[inline] pub fn input(&self) -> &Rc<AnalyserNode> {&self.input}

    // TODO: correctly readjust the graph when shrinked in the UI
    pub fn handle_event(&mut self, event: &AppEvent, ctx: &AppContext) -> JsResult<()> {
        Ok(match event {
            AppEvent::Resize => {
                let canvas: HtmlCanvasElement = self.canvas.cast().to_js_result(loc!())?;
                let [w, h] = canvas.client_size().map(|x| x as u32);
                canvas.set_width(w);
                canvas.set_height(h);
                self.width = w;
                self.height = h;
                self.in_data.resize(w as usize, 0);
                self.out_data.resize(w as usize * w as usize, Self::BG);
            }

            AppEvent::Frame(..) if ctx.play_since().is_finite() => {
                self.out_data.rotate_right(1);
                self.input.get_byte_frequency_data(&mut self.in_data);
                for (&src, dst) in self.in_data.iter().zip(self.out_data.every_nth_mut(self.width as usize)) {
                    *dst = unsafe {*self.gradient.get_unchecked(src as usize)};
                }

                let out = unsafe{from_raw_parts(self.out_data.as_ptr().cast(), self.out_data.len() * 4)};
                let out = ImageData::new_with_u8_clamped_array(Clamped(out), self.width).add_loc(loc!())?;
                self.canvas.cast::<HtmlCanvasElement>().to_js_result(loc!())?
                    .get_2d_context(loc!())?
                    .put_image_data(&out, 0.0, 0.0).add_loc(loc!())?;
            }

            _ => (),
        })
    }
}

#[derive(Debug, PartialEq, Default)]
pub struct HintHandler {
    main_bar: NodeRef,
    aux_bar: NodeRef
}

impl HintHandler {
    pub fn handle_event(&mut self, event: &AppEvent, _: &AppContext) -> JsResult<()> {
        Ok(match event {
            AppEvent::SetHint(main, aux) => {
                self.main_bar.cast::<HtmlElement>().to_js_result(loc!())?
                    .set_inner_text(main);
                self.aux_bar.cast::<HtmlElement>().to_js_result(loc!())?
                    .set_inner_text(aux);
            }

            AppEvent::FetchHint(e) => {
                let main_bar: HtmlElement = self.main_bar.cast().to_js_result(loc!())?;
                let  aux_bar: HtmlElement = self.aux_bar.cast().to_js_result(loc!())?;
                let mut src: Element = e.target_dyn_into().to_js_result(loc!())?;

                loop {
                    let dataset = if let Some(x) = src.dyn_ref::<HtmlElement>() {
                        x.dataset()
                    } else if let Some(x) = src.dyn_ref::<SvgElement>() {
                        x.dataset()
                    } else {
                        main_bar.set_inner_text("");
                        aux_bar.set_inner_text("");
                        break;
                    };
                    if let Some(main) = dataset.get("mainHint") {
                        main_bar.set_inner_text(&main);
                        aux_bar.set_inner_text(&dataset.get("auxHint").unwrap_or_default());
                        break;
                    }
                    if let Some(parent) = src.parent_element() {
                        src = parent
                    } else {
                        break default()
                    }
                };
            }

            _ => ()
        })
    }

    #[inline] pub fn main_bar(&self) -> &NodeRef {&self.main_bar}

    #[inline] pub fn aux_bar(&self) -> &NodeRef {&self.aux_bar}
}

/// data that can be edited with a generic graph editor defined below
pub trait Graphable: Sized + Ord {
    /// the name of the plane that will be displayed as a hint when hovered over it
    const EDITOR_NAME: &'static str;
    /// bounds for the points along the X axis
    const X_BOUND: Range<R64> = r64![0.0] .. R64::INFINITY;
    /// bounds for the scale of the X axis of the graph
    const SCALE_X_BOUND: Range<R64> = r64![3.0] .. r64![30.0];
    /// bounds for the offset of the X axis of the graph
    const OFFSET_X_BOUND: Range<R64> = r64![-1.0] .. R64::INFINITY;
    /// bounds for the points along the Y axis
    const Y_BOUND: Range<R64>;
    /// bounds for the scale of the Y axis of the graph
    const SCALE_Y_BOUND: Range<R64>;
    /// bounds for the offset of the Y axis of the graph
    const OFFSET_Y_BOUND: Range<R64>;
    /// snap step along the Y axis (snap step along the X axis is set globally)
    const Y_SNAP: R64;
    /// type of the inner data of the point, can be whatever and,
    /// unlike the coordinates, can be freely mutated
    type Inner;

    /// inner data of the point
    fn inner(&self) -> &Self::Inner;
    /// mutable inner data of the point
    fn inner_mut(&mut self) -> &mut Self::Inner;
    /// location of the point in user coordinates
    fn loc(&self) -> [R64; 2];
    /// change the location of the point in user coordinates when moved in the UI
    /// `meta` signifies whether the meta key was held while moving the point
    fn move_point(point: Result<&mut Self, &mut [R64; 2]>, delta: [R64; 2], meta: bool);
    /// returns `true` if the given user coordinates are inside the hitbox of the point
    fn in_hitbox(&self, point: [R64; 2]) -> bool;

    ////// HANDLERS
    /// Handle points being moved in the UI.
    #[allow(unused_variables)] #[inline] fn on_move(
        editor: &mut GraphEditor<Self>,
        ctx: &AppContext,
        cursor: Cursor,
        delta: [R64; 2]
    ) {} 
    /// Handle the editor space being clicked, i.e. pressed & released.
    /// The current selection will be available in the `editor` itself.
    #[allow(unused_variables)] #[inline] fn on_click(
        editor: &mut GraphEditor<Self>,
        ctx: &AppContext,
        cursor: Cursor,
        pressed_at:    impl Deref<Target = [R64; 2]>,
        released_at:   impl Deref<Target = [R64; 2]>,
        old_selection: Cow<'_, [usize]>
    ) {}
    /// Handle cursor moving across the editor plane (not across a selection or a point).
    /// `loc` is in user coordinates; the location in canvas coordinates is in `cursor`.
    /// `first` signifies whether this call is the first consecutive one.
    /// Note: if shift's held down, hovering over a point/selection won't count as such, and this
    /// Handler will still be called instead of `on_(point/selection)_hover`.
    #[allow(unused_variables)] #[inline] fn on_plane_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &AppContext,
        cursor: Cursor,
        loc: impl Deref<Target = [R64; 2]>,
        first: bool
    ) {}
    /// Handle cursor moving across a point on the editor plane.
    /// `point_id` is the index of the point in the editor.
    /// `first` signifies whether this call is the first consecutive one.
    #[allow(unused_variables)] #[inline] fn on_point_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &AppContext,
        cursor: Cursor,
        point_id: usize,
        first: bool
    ) {}
    /// Handle cursor moving across a selection on the editor plane.
    /// The selection itself is available in `editor`.
    /// `first` signifies whether this call is the first consecutive one.
    #[allow(unused_variables)] #[inline] fn on_selection_hover(
        editor: &mut GraphEditor<Self>,
        ctx: &AppContext,
        cursor: Cursor,
        first: bool
    ) {}

    /// `loc` is in user coordinates
    fn fmt_loc(loc: [R64; 2]) -> String;

    /// visual representation of the point's hitbox
    /// `mapper` maps user coordinates to canvas coordinates, all vertices in the
    /// returned `Path2d` must be the result of the `mapper` function
    fn draw(&self, next: Option<&Self>, mapper: impl Fn([R64; 2]) -> [R64; 2]) -> JsResult<Path2d>;
    /// the canvas's coordinate space
    /// the function will be called every time the user 
    #[inline] fn canvas_coords(canvas: &HtmlCanvasElement) -> JsResult<[u32; 2]> {
        Ok([canvas.client_width() as u32, canvas.client_height() as u32])
    }
}

/// a special reference wrapper: access to everything is immutable,
/// except for the inner value, which is mutable
pub struct GraphPointView<'a, T: Graphable>(&'a mut T);

impl<'a, T: Graphable> Deref for GraphPointView<'a, T> {
    type Target = T;
    #[inline] fn deref(&self) -> &Self::Target {self.0}
}

impl<'a, T: Graphable> GraphPointView<'a, T> {
    #[inline] pub fn inner(&mut self) -> &mut T::Inner {
        self.0.inner_mut()
    }
}

// types as coordinate space hints:
/// in canvas coordinates with plane offset
type OffsetCanvasPoint = Point;
/// in user coordinates, aligned to snap step and limited to bounds specified by `Graphable::(X/Y)_BOUND`
type ConfinedAlignedUserPoint = [R64; 2];

#[derive(Default, Debug, Clone, PartialEq, Eq)]
enum Focus {
    #[default] None,
    Zoom{init_offset: Point, pivot: OffsetCanvasPoint, init_scale: [R64; 2]},
    Plane{origin: ConfinedAlignedUserPoint, press_time: Secs},
    Point{id: usize, last_loc: ConfinedAlignedUserPoint, origin: ConfinedAlignedUserPoint},
    Selection{origin: ConfinedAlignedUserPoint, end: ConfinedAlignedUserPoint}
}

static GRAPH_EDITOR_COUNT: WasmCell<Cell<usize>> = WasmCell::new(Cell::new(AnyGraphEditor::INVALID_ID + 1));

/// base of `GraphEditor`, distinguished from the former to ease working with cases when the
/// contained point type doesn't matter
#[derive(Debug, Clone, PartialEq, Default)]
pub struct AnyGraphEditor {
    canvas: NodeRef,
    offset: Point,
    scale: [R64; 2],
    focus: Focus,
    selection: Vec<usize>,
    selection_origin: ConfinedAlignedUserPoint,
    selection_size: [R64; 2],
    last_cursor: Cursor,
    redraw: bool,
    changed_focus: bool,
    grid: Option<(Path2d, [R64; 2])>,
    id: usize
}

impl AnyGraphEditor {
    const FONT: &str = "20px consolas";
    const BG_STYLE: &str = "#232328";
    const MG_STYLE: &str = "#333338";
    const FG_STYLE: &str = "#0069E1";
    const LINE_WIDTH: f64 = 3.0;
    /// an ID that's guaranteed to never be used by any graph editor
    const INVALID_ID: usize = 0;

    #[inline] fn id(&self) -> usize {self.id}

    #[inline] pub fn last_cursor(&self) -> Option<Cursor> {
        matches!(self.focus, Focus::None).not().then_some(self.last_cursor)
    }

    #[inline] pub fn canvas(&self) -> &NodeRef {&self.canvas}

    #[inline] pub fn selection(&self) -> &[usize] {&self.selection}
}

#[derive(Debug, Clone, PartialEq)]
pub struct GraphEditor<T: Graphable> {
    inner: AnyGraphEditor,
    data: Vec<T>
}

impl<T: Graphable> Deref for GraphEditor<T> {
    type Target = AnyGraphEditor;
    #[inline] fn deref(&self) -> &Self::Target {&self.inner}
}

impl<T: Graphable> DerefMut for GraphEditor<T> {
    #[inline] fn deref_mut(&mut self) -> &mut Self::Target {&mut self.inner}
}

impl<T: Graphable> GraphEditor<T> {
    #[inline] pub fn new(data: Vec<T>) -> Self {
        Self{data,
            inner: AnyGraphEditor{
                scale: [T::SCALE_X_BOUND, T::SCALE_Y_BOUND]
                    .map(|x| x.to_pair().mul(&[0.75, 0.25]).sum()),
                ..default()}}
    }

    #[inline] pub fn len(&self) -> usize {self.data.len()}

    #[inline] pub fn get(&self, index: usize) -> Option<&T> {
        self.data.get(index)
    }

    #[inline] pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        self.data.get_unchecked(index)
    }

    #[inline] pub unsafe fn get_unchecked_aware(&self, index: usize) -> SliceRef<'_, T> {
        SliceRef::raw(self.get_unchecked(index), index)
    }

    #[inline] pub unsafe fn get_unchecked_mut(&mut self, index: usize)
    -> GraphPointView<'_, T> {
        GraphPointView(self.data.get_unchecked_mut(index))
    }

    #[inline] pub unsafe fn first_unchecked(&self) -> &T {
        &*self.data.as_ptr()
    }

    #[inline] pub unsafe fn last_unchecked(&self) -> &T {
        unsafe{self.data.last().unwrap_unchecked()}
    }

    #[inline] pub fn iter_mut(&mut self) -> impl Iterator<Item=GraphPointView<'_, T>> {
        self.data.iter_mut().map(GraphPointView)
    }

    /// returns the index at which the new point will be available
    #[inline] pub fn add_point(&mut self, point: T) -> usize {
        self.redraw = true;
        let to = self.data.push_sorted(point);
        SliceMove{from: self.data.len(), to}.apply(&mut self.selection);
        to
    }

    #[inline] pub fn remove_points(&mut self, to_remove: &[usize]) -> JsResult<()> {
        self.redraw = true;
        js_assert!(to_remove.is_sorted())?;
        let GraphEditor{inner, data} = self;
        let mut ids_iter = inner.selection.iter_mut().rev();
        for &id in to_remove.iter().rev() {
            data.try_remove(id).to_js_result(loc!())?;
            let rem = loop {
                let Some(x) = ids_iter.next() else {break 0};
                match id.cmp(x) {
                    Ordering::Less =>
                        *x -= 1,
                    Ordering::Equal =>
                        break ids_iter.len().pipe(|x| unsafe{inner.selection.remove_unchecked(x); x}),
                    Ordering::Greater =>
                        break ids_iter.len()
                }
            };
            ids_iter = inner.selection.get_mut(..rem).unwrap_or(&mut []).iter_mut().rev();
        }
        Ok(())
    }

    #[inline] pub fn force_redraw(&mut self) {self.redraw = true}

    /// must be called when a canvas has just been bound or its dimensions have been changed
    pub fn init(&mut self) -> JsResult<()> {
        let canvas: HtmlCanvasElement = self.canvas.cast().to_js_result(loc!())?;
        let [w, h] = T::canvas_coords(&canvas).add_loc(loc!())?;
        canvas.set_width(w);
        canvas.set_height(h);
        self.scale = self.scale.map(|x| x.ceil_to(r64![2.0]));
        self.grid = None;
        if self.offset.x <= 0 {
            self.offset.x = (T::OFFSET_X_BOUND.start * R64::from(w) / self.scale[0]).into();
        }
        if self.offset.y <= 0 {
            self.offset.y = (T::OFFSET_Y_BOUND.start * R64::from(h) / self.scale[1]).into();
        }
        let ctx = canvas.get_2d_context(loc!())?;
        ctx.set_font(AnyGraphEditor::FONT);
        ctx.set_line_width(AnyGraphEditor::LINE_WIDTH);
        Ok(self.redraw = true)
    }

    #[inline] fn point_by_pos(&self, loc: [R64; 2]) -> Option<SliceRef<'_, T>> {
        self.data.iter().enumerate().find(|(_, x)| x.in_hitbox(loc))
            .map(|(id, x)| unsafe{SliceRef::raw(x, id)})
    }

    #[inline] fn point_in_selection(&self, loc: ConfinedAlignedUserPoint) -> bool {
        loc.sub(&self.selection_origin).zip_fold(true, self.selection_size, |r, x, y| r && 0.0 <= *x && x <= y)
    }

    #[inline] fn set_zoom_focus(&mut self, ctx: &AppContext, cursor: Cursor, loc: impl Deref<Target = [R64; 2]>) {
        let first = !matches!(self.focus, Focus::Plane{..});
        self.focus = Focus::Zoom{init_offset: self.offset,
            pivot: cursor.point + self.offset,
            init_scale: self.scale};
        self.changed_focus = true;
        T::on_plane_hover(self, ctx, cursor, loc, first)
    }

    #[inline] fn set_plane_focus(&mut self, ctx: &AppContext, cursor: Cursor, loc: impl Deref<Target = [R64; 2]>) {
        self.focus = Focus::Plane{origin: default(), press_time: default()};
        self.changed_focus = true;
        T::on_plane_hover(self, ctx, cursor, loc, true);
    }

    #[inline] fn set_point_focus(&mut self, ctx: &AppContext, cursor: Cursor, id: usize) {
        self.focus = Focus::Point{id, last_loc: default(), origin: default()};
        self.changed_focus = true;
        T::on_point_hover(self, ctx, cursor, id, true)
    }

    #[inline] fn set_selection_focus(&mut self, ctx: &AppContext, cursor: Cursor) {
        self.focus = Focus::Selection{origin: default(), end: default()};
        self.changed_focus = true;
        T::on_selection_hover(self, ctx, cursor, true)
    }

    fn handle_hover(&mut self, cursor: Option<Cursor>, ctx: &mut AppContext) -> JsResult<()> {
        let Some(cursor) = cursor else {
            self.focus = Focus::None;
            self.changed_focus = true;
            return Ok(())
        };

        let size = self.canvas.cast::<HtmlCanvasElement>()
            .to_js_result(loc!())?.size();
        let snap_step = [ctx.snap_step(), T::Y_SNAP];
        let step = &R64::array_from(size).div(&self.scale);

        let cursor_point_user = LazyCell::new({
            let off = self.offset;
            move || R64::array_from(cursor.point + off).div(step)
        });
        let cursor_point_user_aligned_confined = LazyCell::new(||
            [T::X_BOUND, T::Y_BOUND].fit(cursor_point_user.floor_to(snap_step)));

        match &mut self.inner.focus {
            Focus::None => self.set_plane_focus(ctx, cursor, Alias(&cursor_point_user_aligned_confined)),

            Focus::Zoom{init_offset, init_scale, pivot} => if cursor.left {
                if !self.inner.last_cursor.left {
                    *pivot = cursor.point + self.inner.offset;
                    *init_scale = self.inner.scale;
                    *init_offset = self.inner.offset;
                } else {
                    self.inner.redraw = true;
                }
                let point = cursor.point + *init_offset - *pivot;
                if !T::SCALE_X_BOUND.is_empty() {
                    self.inner.scale[0] = T::SCALE_X_BOUND.fit(r64![50.0] / size[1] * point.x + init_scale[0]);
                    self.inner.offset.x = ((init_scale[0] - self.inner.scale[0]) * pivot.x / self.inner.scale[0] + init_offset.x).into();
                }
                if !T::SCALE_Y_BOUND.is_empty() {
                    self.inner.scale[1] = T::SCALE_Y_BOUND.fit(r64![50.0] / size[0] * point.y + init_scale[1]);
                    self.inner.offset.y = ((init_scale[1] - self.inner.scale[1]) * pivot.y / self.inner.scale[1] + init_offset.y).into();
                }
            } else {
                if self.inner.last_cursor.left {
                    ctx.register_action(AppAction::ZoomPlane{
                        editor_id: self.inner.id,
                        from_offset: *init_offset, from_scale: *init_scale,
                        to_offset: self.inner.offset, to_scale: self.inner.scale})
                }
                if cursor.shift {
                    *pivot = cursor.point;
                    self.inner.redraw = true;
                } else {
                    self.set_plane_focus(ctx, cursor, Alias(&cursor_point_user_aligned_confined));
                }
            }

            Focus::Plane{origin, press_time} => match *cursor {
                Buttons{shift: true, ..} => self.set_zoom_focus(ctx, cursor, Alias(&cursor_point_user_aligned_confined)),

                Buttons{left: false, meta, ..} => if self.inner.last_cursor.left {
                    let cur_loc = *cursor_point_user;
                    if self.inner.last_cursor.meta {

                        let origin = *origin;
                        let end = *cursor_point_user_aligned_confined;
                        let old_selection = self.inner.selection.clone().into_boxed_slice();
                        let area = [origin, end].flipped().map(|x| (x[0] .. x[1]).ordered());

                        self.inner.selection_origin = area.clone().map(|x| x.start);
                        self.inner.selection_size = area.clone().map(|x| x.end - x.start);
                        self.inner.selection = self.data.iter().enumerate()
                            .filter_map(|(id, x)| x.loc().array_check_in(&area).map(|_| id))
                            .collect();
                        T::on_click(self, ctx, cursor,
                            &origin, &end,
                            Cow::Borrowed(&old_selection));

                        ctx.register_action(AppAction::ChangeSelection{
                            editor_id: self.inner.id,
                            ids: old_selection, 
                            origin: self.inner.selection_origin,
                            size: self.inner.selection_size});

                        if let Some(p) = self.point_by_pos(cur_loc) {
                            self.set_point_focus(ctx, cursor, p.index());
                        } else {
                            self.set_plane_focus(ctx, cursor, &end);
                        }
                    } else {
                        let ids = self.inner.selection.clone();
                        if *ctx.now() - **press_time > 0.33 {
                            T::on_click(self, ctx, cursor,
                                Alias(&cursor_point_user_aligned_confined), Alias(&cursor_point_user_aligned_confined),
                                Cow::Owned(ids))
                        } else {
                            self.inner.selection.clear();
                            let size = take(&mut self.inner.selection_size);
                            T::on_click(self, ctx, cursor,
                                Alias(&cursor_point_user_aligned_confined), Alias(&cursor_point_user_aligned_confined),
                                Cow::Borrowed(&ids));
                            ctx.register_action(AppAction::ChangeSelection{
                                editor_id: self.inner.id,
                                ids: ids.into_boxed_slice(),
                                size,
                                origin: self.inner.selection_origin})
                        }
                    }
                } else if self.point_in_selection(*cursor_point_user) {
                    self.set_selection_focus(ctx, cursor);
                } else if let Some(p) = self.point_by_pos(*cursor_point_user) {
                    self.set_point_focus(ctx, cursor, p.index());
                } else {
                    self.inner.redraw |= self.inner.last_cursor.meta | meta;
                }

                Buttons{left: true, meta, ..} => if meta {
                    if !self.inner.last_cursor.left {
                        *origin = *cursor_point_user_aligned_confined;
                    } else {self.inner.redraw = true}
                } else {
                    if !self.inner.last_cursor.left {
                        *press_time = ctx.now();
                    } else {self.inner.redraw = true}

                    if !T::OFFSET_X_BOUND.is_empty() {
                        self.inner.offset.x = T::OFFSET_X_BOUND.map(|x| x * step[0])
                            .extend(self.inner.offset.x).fit(self.inner.offset.x + self.inner.last_cursor.point.x - cursor.point.x);
                    }
                    if !T::OFFSET_Y_BOUND.is_empty() {
                        self.inner.offset.y = T::OFFSET_Y_BOUND.map(|y| y * step[1])
                            .extend(self.inner.offset.y).fit(self.inner.offset.y + self.inner.last_cursor.point.y - cursor.point.y);
                    }
                }
            }

            Focus::Point{id, last_loc, origin} => if cursor.shift {
                self.set_zoom_focus(ctx, cursor, Alias(&cursor_point_user_aligned_confined));
            } else if cursor.left {
                let delta = if !self.inner.last_cursor.left {
                    *last_loc = *cursor_point_user_aligned_confined;
                    *origin = *last_loc;
                    default()
                } else {
                    let new = *cursor_point_user_aligned_confined;
                    new.sub(&replace(last_loc, new))
                };
                if *delta.sum::<R64>() != 0.0 {
                    self.inner.redraw = true;
                    T::move_point(Ok(unsafe{self.data.get_unchecked_mut(*id)}), delta, cursor.meta);
                    unsafe{self.data.reorder_unchecked(*id)}.apply(from_mut(id));
                    T::on_move(self, ctx, cursor, delta)
                }
            } else {
                let p = unsafe{self.data.get_unchecked_aware(*id)};
                match (self.inner.last_cursor.left, p.in_hitbox(*cursor_point_user)) {
                    (false, false) => self.set_plane_focus(ctx, cursor, Alias(&cursor_point_user_aligned_confined)),
                    (false, true) => (),
                    (true, false) => {
                        let origin = *origin;
                        let old_selection = take(&mut self.inner.selection);
                        self.set_plane_focus(ctx, cursor, Alias(&cursor_point_user_aligned_confined));
                        T::on_click(self, ctx, cursor,
                            &origin, Alias(&cursor_point_user_aligned_confined),
                            Cow::Owned(old_selection))
                    }
                    (true, true) => {
                        let origin = *origin;
                        let old_selection = replace(&mut self.inner.selection, vec![*id]);
                        self.set_selection_focus(ctx, cursor);
                        T::on_click(self, ctx, cursor,
                            &origin, Alias(&cursor_point_user_aligned_confined),
                            Cow::Owned(old_selection))
                    }
                }
            }

            Focus::Selection{origin, end} => if cursor.shift {
                self.set_zoom_focus(ctx, cursor, Alias(&cursor_point_user_aligned_confined));
            } else if cursor.left {
                let delta = if !self.inner.last_cursor.left {
                    *end = *cursor_point_user_aligned_confined;
                    *origin = *end;
                    default()
                } else {
                    let new = *cursor_point_user_aligned_confined;
                    new.sub(&replace(end, new))
                };
                if *delta.sum::<R64>() != 0.0 {
                    self.inner.redraw = true;
                    for (ids, id) in self.inner.selection.iter_mut_with_ctx() {
                        T::move_point(Ok(unsafe{self.data.get_unchecked_mut(id)}), delta, cursor.meta);
                        unsafe{self.data.reorder_unchecked(id)}.apply(ids);
                    }
                    T::move_point(Err(&mut self.inner.selection_origin), delta, cursor.meta);
                    self.inner.selection.sort_unstable();
                    T::on_move(self, ctx, cursor, delta)
                }
            } else if self.inner.last_cursor.left {
                let selection = self.inner.selection.clone();
                let origin = take(origin);
                *end = default();
                T::on_click(self, ctx, cursor,
                    &origin, Alias(&cursor_point_user_aligned_confined),
                    Cow::Owned(selection))
            } else if !self.point_in_selection(*cursor_point_user) {
                if let Some(p) = self.point_by_pos(*cursor_point_user) {
                    self.set_point_focus(ctx, cursor, p.index());
                } else {
                    self.set_plane_focus(ctx, cursor, Alias(&cursor_point_user_aligned_confined));
                }
            }
        };

        if self.inner.changed_focus {
            self.inner.changed_focus = false
        } else {
            match self.inner.focus {
                Focus::None => (),
                Focus::Zoom{..} | Focus::Plane{..} =>
                    T::on_plane_hover(self, ctx, cursor, cursor_point_user_aligned_confined, false),
                Focus::Point{id, ..} =>
                    T::on_point_hover(self, ctx, cursor, id, false),
                Focus::Selection{..} =>
                    T::on_selection_hover(self, ctx, cursor, false)
            }
        }

        let old_buttons = *replace(&mut self.inner.last_cursor, cursor);
        self.inner.redraw |= *cursor != old_buttons;
        Ok(())
    }

    /// an offset of 0 is assumed
    /// the returned array are the actual bounds of the rendered grid in user coordinates
    fn draw_grid(canvas_size: [R64; 2], step: [R64; 2], scale: [R64; 2]) -> JsResult<(Path2d, [R64; 2])> {
        let res = Path2d::new().add_loc(loc!())?;
        let steps: [usize; 2] = [T::X_BOUND.end, T::Y_BOUND.end].mul(&step)
            .zip(canvas_size, min).div(&canvas_size).mul(&scale)
            .into_array();

        for x in 0 .. steps[0] {
            for y in (0 .. steps[1]).step_by(2) {
                let [x, y] = [step[0] * x, step[1] * y];
                res.rect(*x + AnyGraphEditor::LINE_WIDTH, *y, *step[0] - AnyGraphEditor::LINE_WIDTH, *step[1]);
                res.rect(*x, *y + *step[1], AnyGraphEditor::LINE_WIDTH, *step[1]);
            }
        }

        Ok((res, scale))
    }

    pub fn handle_event(&mut self, event: &AppEvent, ctx: &mut AppContext) -> JsResult<()> {
        Ok(match event {
            AppEvent::Enter(id, e) | AppEvent::Hover(id, e) if *id == self.id =>
                self.handle_hover(Some(e.try_into().add_loc(loc!())?), ctx).add_loc(loc!())?,

            AppEvent::Focus(id, e) if *id == self.id => {
                e.target_dyn_into::<Element>().to_js_result(loc!())?
                    .set_pointer_capture(e.pointer_id()).add_loc(loc!())?;
                self.handle_hover(Some(e.try_into().add_loc(loc!())?), ctx).add_loc(loc!())?;
            }

            AppEvent::KeyPress(id, e) | AppEvent::KeyRelease(id, e) if *id == self.id =>
                self.handle_hover(Some(self.last_cursor + e), ctx).add_loc(loc!())?,

            AppEvent::Leave(id) if *id == self.id =>
                self.handle_hover(None, ctx).add_loc(loc!())?,

            AppEvent::Resize =>
                self.init().add_loc(loc!())?,

            AppEvent::AudioStarted(_) => self.redraw = true,

            AppEvent::Frame(_) if self.redraw => {
                let canvas: HtmlCanvasElement = self.canvas().cast().to_js_result(loc!())?;
                let size = canvas.size().map(R64::from);
                let snap_step = [ctx.snap_step(), T::Y_SNAP];
                let canvas_ctx = canvas.get_2d_context(loc!())?;

                let step = &size.div(&self.scale);
                let offset_x = R64::from(-self.offset.x)
                    % (self.offset.x > (T::X_BOUND.start * step[0]).into())
                        .choose(step[0], R64::INFINITY);
                let offset_y = R64::from(-self.offset.y)
                    % (self.offset.y > (T::Y_BOUND.start * step[1]).into())
                    .choose(step[1] * 2.0, R64::INFINITY);

                let offset = |loc| loc + self.inner.offset;
                let to_user = |loc| R64::array_from(offset(loc)).div(step);
                let to_aligned_canvas = |loc: Point| loc.floor_to(snap_step.mul(step).into());
                let confine = |x| [T::X_BOUND, T::Y_BOUND].fit(x);

                canvas_ctx.set_fill_style(&AnyGraphEditor::BG_STYLE.into());
                canvas_ctx.fill_rect(0.0, 0.0, *size[0], *size[1]);

                let (grid, original_scale) = self.inner.grid.get_or_try_insert(||
                    Self::draw_grid(size, *step, self.inner.scale).add_loc(loc!()))?;
                let grid_scale = original_scale.div(&self.inner.scale);
                let repetitions = self.inner.scale.sub(&[offset_x, offset_y].div(step))
                    .div(original_scale)
                    .map(|x| usize::from(x.ceil()));

                canvas_ctx.set_fill_style(&AnyGraphEditor::MG_STYLE.into());
                canvas_ctx.transform(*grid_scale[0], 0.0, 0.0, *grid_scale[1], *offset_x, *offset_y).add_loc(loc!())?;

                for _ in 0 .. repetitions[1] {
                    for _ in 0 .. repetitions[0] {
                        canvas_ctx.fill_with_path_2d(grid);
                        canvas_ctx.translate(*size[0], 0.0).add_loc(loc!())?;
                    }
                    canvas_ctx.translate(-*size[0] * repetitions[0] as f64, *size[1]).add_loc(loc!())?;
                }
                canvas_ctx.reset_transform().add_loc(loc!())?;

                let points = Path2d::new().add_loc(loc!())?;
                let mapper = |loc: [R64; 2]| loc.mul(step).sub(&R64::array_from(self.inner.offset));
                for [this, next] in self.data.array_windows::<2>() {
                    points.add_path(&this.draw(Some(next), mapper).add_loc(loc!())?);
                }
                if let Some(last) = self.data.last() {
                    points.add_path(&last.draw(None, mapper).add_loc(loc!())?);
                }
                canvas_ctx.fill_with_path_2d(&points);
                canvas_ctx.set_stroke_style(&AnyGraphEditor::FG_STYLE.into());
                canvas_ctx.stroke_with_path(&points);

                let [x, y] = mapper(self.selection_origin);
                let [w, h] = self.selection_size.mul(step);
                canvas_ctx.set_line_dash(eval_once!(JsValue: js_array![number 10.0, number 10.0])).add_loc(loc!())?;
                canvas_ctx.stroke_rect(*x, *y, *w, *h);
                canvas_ctx.set_line_dash(eval_once!(JsValue: js_array![])).add_loc(loc!())?;

                if ctx.play_since().is_finite() {
                    canvas_ctx.set_fill_style(&AnyGraphEditor::FG_STYLE.into());
                    let play_at = (ctx.now() - ctx.play_since()).secs_to_beats(ctx.bps());
                    canvas_ctx.fill_rect(*play_at * *step[0] - self.offset.x as f64, 0.0, 3.0, *size[1]);
                } else {
                    self.inner.redraw = false;
                }

                match self.focus {
                    Focus::Zoom{pivot, init_offset, ..} => if self.last_cursor.left {
                        let [x, y] = (pivot - init_offset).map(|x| x as f64);
                        canvas_ctx.begin_path();
                        canvas_ctx.move_to(x - 10.0, y);
                        canvas_ctx.line_to(x + 10.0, y);
                        canvas_ctx.move_to(x, y - 10.0);
                        canvas_ctx.line_to(x, y + 10.0);
                        canvas_ctx.set_stroke_style(&AnyGraphEditor::FG_STYLE.into());
                        canvas_ctx.stroke();
                    } else {
                        canvas_ctx.set_text_align("left");
                        canvas_ctx.set_text_baseline("bottom");
                        canvas_ctx.set_fill_style(&AnyGraphEditor::FG_STYLE.into());
                        canvas_ctx.fill_text(&T::fmt_loc(confine(to_user(pivot))), 5.0, *size[1] - 5.0)
                            .add_loc(loc!())?;
                    }

                    Focus::Plane{origin, ..} if self.last_cursor.meta => if self.last_cursor.left {
                        let cur = to_aligned_canvas(self.last_cursor.point);
                        let origin = mapper(origin).map(|x| *x);
                        canvas_ctx.set_line_dash(eval_once!(JsValue: js_array![number 10.0, number 10.0])).add_loc(loc!())?;
                        canvas_ctx.stroke_rect(origin[0], origin[1], cur.x as f64 - origin[0], cur.y as f64 - origin[1]);
                        canvas_ctx.set_line_dash(eval_once!(JsValue: js_array![])).add_loc(loc!())?;
                    } else {
                        let [x, y] = self.last_cursor.point.map(|x| x as f64);
                        canvas_ctx.set_line_dash(eval_once!(JsValue: js_array![number 10.0, number 10.0])).add_loc(loc!())?;
                        canvas_ctx.begin_path();
                        canvas_ctx.move_to(-*size[0], y);
                        canvas_ctx.line_to( *size[0], y);
                        canvas_ctx.move_to(x, -*size[1]);
                        canvas_ctx.line_to(x,  *size[1]);
                        canvas_ctx.stroke();
                        canvas_ctx.set_line_dash(eval_once!(JsValue: js_array![])).add_loc(loc!())?;
                    }
                    
                    _ => ()
                }
            }

            _ => (),
        })
    }
}

// TODO: make this not need `<T>` somehow
#[derive(Debug, PartialEq, Properties)]
pub struct GraphEditorCanvasProps<T: Graphable> {
    pub emitter: Callback<AppEvent>,
    pub editor: Shared<GraphEditor<T>>,
    pub id: Option<&'static str>
}

#[function_component(GraphEditorCanvas)]
pub fn f<T: Graphable>(props: &GraphEditorCanvasProps<T>) -> Html {
    let GraphEditorCanvasProps{emitter, editor, id} = props;
    match editor.try_borrow().to_js_result(loc!()) {
        Ok(editor) => {
            let (canvas_id, id) = (*id, editor.id());
            html!{<canvas ref={editor.canvas().clone()} id={canvas_id}
                onpointerdown={emitter.reform(move  |e| AppEvent::Focus(id, e))}
                onpointerup={emitter.reform(move    |e| AppEvent::Hover(id, MouseEvent::from(e)))}
                onpointermove={emitter.reform(move  |e| AppEvent::Hover(id, MouseEvent::from(e)))}
                onpointerenter={emitter.reform(move |e| AppEvent::Enter(id, MouseEvent::from(e)))}
                onpointerout={emitter.reform(move   |_| AppEvent::Leave(id))}/>}
        }
        Err(err) => {
            report_err(err);
            html!{"Error"}
        }
    }
}
