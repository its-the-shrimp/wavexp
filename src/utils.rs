use wasm_bindgen::JsCast;

pub trait Check: Sized {
	#[inline] fn check<F>(self, f: F) -> Result<Self, Self>
    where F: FnOnce(&Self) -> bool {
		if f(&self) {Ok(self)} else {Err(self)}
	}

    #[inline] fn is_in<R>(self, range: R) -> Result<Self, Self>
	where Self: PartialOrd, R: std::ops::RangeBounds<Self> {
		if range.contains(&self) {Ok(self)} else {Err(self)}
	}

	#[inline] fn not_in<R>(self, range: R) -> Result<Self, Self>
	where Self: PartialOrd, R: std::ops::RangeBounds<Self> {
		if !range.contains(&self) {Ok(self)} else {Err(self)}
	}
}
impl<T> Check for T {}

pub trait Tee: Sized {
	#[inline] fn tee<F>(self, f: F) -> Self where F: FnOnce(&Self) {
        f(&self); self
	}

    #[inline] fn js_log<'a>(self, label: &'a str) -> Self 
    where Self: std::fmt::Debug {
        js_log!("{}{:?}", label, &self); self
    }
}
impl<T> Tee for T {}

pub trait Pipe: Sized {
	#[inline]
	fn pipe<O, F>(self, f: F) -> O where F: FnOnce(Self) -> O { f(self) }
}
impl<T> Pipe for T {}

pub trait Drop: Sized {
	#[inline] fn drop(self) {}
}
impl<T> Drop for T {}

pub trait Choose: Sized + Into<bool> {
	#[inline] fn choose<T: Sized>(self, on_true: T, on_false: T) -> T {
		if self.into() {on_true} else {on_false}
	}

    #[inline] fn then_invert<T: std::ops::Not<Output=T>>(self, val: T) -> T {
        if self.into() {std::ops::Not::not(val)} else {val}
    }
}
impl Choose for bool {}

#[allow(non_camel_case_types)]
#[allow(dead_code)]
pub mod js_types {
	pub type bool = js_sys::Boolean;
	pub type number = js_sys::Number;
	pub type str = js_sys::JsString;
}

#[macro_export]
macro_rules! js_obj {
	($($t:ident $k:ident : $v:expr),*) => {
		$crate::wasm_bindgen::JsValue::from($crate::js_sys::Map::new()
			$( .set(&$crate::utils::js_types::str::from(stringify!($k)).into(),
				&*$crate::utils::js_types::$t::from($v)) )*)
	}
}
pub use js_obj;

#[macro_export]
macro_rules! js_log {
	($arg:literal) => {
		$crate::web_sys::console::log_1(&$arg.to_owned().into())
	};
	($f:literal, $($arg:expr),*) => {
		$crate::web_sys::console::log_1(&format!($f, $($arg),*).into())
	}
}
pub use js_log;

use crate::MainCmd;

pub fn window() -> crate::web_sys::Window {
	unsafe {web_sys::window().unwrap_unchecked()}
}

pub fn document() -> crate::web_sys::Document {
	unsafe {web_sys::window().unwrap_unchecked().document().unwrap_unchecked()}
}


pub fn get_canvas_ctx(name: &str, antialias: bool, alpha: bool)
-> JsResult<(f64, f64, web_sys::CanvasRenderingContext2d)> {
    let res: web_sys::HtmlCanvasElement = document().element_dyn_into(name)?;
    Ok((res.width() as f64, res.height() as f64,
        res.get_context_with_context_options("2d", &js_obj!{bool alpha: alpha, bool antialias: antialias})?
            .ok_or_js_error_with(|| format!("rendering context not found for canvas #{}", name))?
            .unchecked_into::<web_sys::CanvasRenderingContext2d>()))
}

pub fn sync_canvas(name: &str) -> JsResult<()> {
    let err: JsResult<!> = try {
        let comp = document().get_element_by_id(name)
            .ok_or_js_error("element not found")?;
        let comp = comp.dyn_into::<web_sys::HtmlCanvasElement>().ok()
            .ok_or_js_error("the element is not a <canvas>")?;
        comp.set_height((comp.client_height() as f64 / comp.client_width() as f64 * 300.0) as u32);
        return Ok(())
    };
    Err(err.explain_err_with(|| format!("syncing the dimensions of the element #{}", name)).into_err())
}

fn to_error_with_msg(err: wasm_bindgen::JsValue, msg: &str) -> wasm_bindgen::JsValue {
    let s = format!("error while {}: {}", msg, 
        match err.dyn_into::<js_sys::Error>() {
            Ok(val) => val.message().into(),
            Err(val) => js_sys::Object::from(val).to_string()});
    js_sys::Error::new(&s).into()
}

pub type JsResult<T> = Result<T, wasm_bindgen::JsValue>;

pub trait OkOrJsError<T>: Sized {
    fn ok_or_js_error_with(self, f: impl FnOnce() -> String) -> JsResult<T>;
    fn ok_or_js_error(self, msg: &str) -> JsResult<T>;
}

impl<T> OkOrJsError<T> for Option<T> {
    #[inline] fn ok_or_js_error_with(self, f: impl FnOnce() -> String) -> JsResult<T> {
        self.ok_or_else(|| js_sys::Error::new(&f()).into())
    }

    #[inline] fn ok_or_js_error(self, msg: &str) -> JsResult<T> {
        self.ok_or_else(|| js_sys::Error::new(msg).into())
    }
}

pub trait ToJsResult<T> {
    fn to_js_result(self) -> JsResult<T>;
}

impl<T, E: std::fmt::Display> ToJsResult<T> for Result<T, E> {
    fn to_js_result(self) -> JsResult<T> {
        self.map_err(|e| e.to_string().into())
    }
}

pub trait JsResultUtils<T>: Sized {
    fn explain_err_with(self, f: impl FnOnce() -> String) -> Self;
    fn explain_err(self, msg: &str) -> Self;
    fn expect_throw_with(self, f: impl FnOnce() -> String) -> T;
	fn expect_throw(self, msg: &str) -> T;
    fn report_err_with(self, f: impl FnOnce() -> String) -> Self;
	fn report_err(self, msg: &str) -> Self;
}

impl<T> JsResultUtils<T> for Result<T, wasm_bindgen::JsValue> {
    #[inline] fn explain_err_with(self, f: impl FnOnce() -> String) -> Self {
        self.map_err(|err| to_error_with_msg(err, &f()))
    }
    #[inline] fn explain_err(self, msg: &str) -> Self {
        self.map_err(|err| to_error_with_msg(err, msg))
    }

    #[inline] fn expect_throw_with(self, f: impl FnOnce() -> String) -> T {
		match self {
			Ok(val)  => val,
			Err(err) => wasm_bindgen::throw_val(to_error_with_msg(err, &f()))}
	}

    #[inline] fn expect_throw(self, msg: &str) -> T {
        match self {
            Ok(val) => val,
            Err(err) => wasm_bindgen::throw_val(to_error_with_msg(err, msg))}
    }

    #[inline] fn report_err_with(self, f: impl FnOnce() -> String) -> Self {
        self.inspect_err(|err|
            MainCmd::ReportError(to_error_with_msg(err.clone(), &f())).send())
    }

    #[inline] fn report_err(self, msg: &str) -> Self {
        self.inspect_err(|err|
            MainCmd::ReportError(to_error_with_msg(err.clone(), msg)).send())
    }
}

pub trait HtmlCanvasExt {
    fn get_2d_context(&self) -> JsResult<web_sys::CanvasRenderingContext2d>;
}

impl HtmlCanvasExt for web_sys::HtmlCanvasElement {
    fn get_2d_context(&self) -> JsResult<web_sys::CanvasRenderingContext2d> {
        Ok(self.get_context("2d")?
            .ok_or_js_error("the element has no rendering context")?
            .unchecked_into::<web_sys::CanvasRenderingContext2d>())
    }
}

pub trait HtmlDocumentExt {
    fn element_dyn_into<T: wasm_bindgen::JsCast>(&self, id: &str) -> JsResult<T>;
}

impl HtmlDocumentExt for web_sys::Document {
    fn element_dyn_into<T: wasm_bindgen::JsCast>(&self, id: &str) -> JsResult<T> {
        Ok(self.get_element_by_id(id).ok_or_js_error_with(|| format!("element #{} not found", id))?
            .dyn_into::<T>().ok().ok_or_js_error_with(|| format!("element #{} is not of type `{}`", id, std::any::type_name::<T>()))?)
    }
}

pub trait SliceExt<T> {
    fn get_saturating<'a>(&'a self, id: usize) -> &'a T;
    fn get_saturating_mut<'a>(&'a mut self, id: usize) -> &'a mut T;
    // the format of the error message would be: <prefix><id><postifx>
    fn get_or_js_error<'a>(&'a self, id: usize, prefix: &str, postfix: &str) -> JsResult<&'a T>;
    fn get_mut_or_js_error<'a>(&'a mut self, id: usize, prefix: &str, postfix: &str) -> JsResult<&'a mut T>;
}

impl<T> SliceExt<T> for [T] {
    #[inline] fn get_saturating<'a>(&'a self, id: usize) -> &'a T {
        unsafe{self.get_unchecked(id.min(self.len() - 1))}
    }

    #[inline] fn get_saturating_mut<'a>(&'a mut self, id: usize) -> &'a mut T {
        unsafe{self.get_unchecked_mut(id.min(self.len() - 1))}
    }

    #[inline] fn get_or_js_error<'a>(&'a self, id: usize, prefix: &str, postfix: &str) -> JsResult<&'a T> {
        self.get(id).ok_or_js_error_with(|| format!("{}{}{}", prefix, id, postfix))
    }

    #[inline] fn get_mut_or_js_error<'a>(&'a mut self, id: usize, prefix: &str, postfix: &str) -> JsResult<&'a mut T> {
        self.get_mut (id).ok_or_js_error_with(|| format!("{}{}{}", prefix, id, postfix))
    }
}

// this exists to circumvent a limiatation on static variables that Rust imposes, which prevents
// them from containing types that don't implement `Sync`. On any other architecture this
// limitation makes sense, but in Webassembly, which doesn't support threading, this limitation is meaningless.
pub struct WasmCell<T>(T);

unsafe impl<T> Sync for WasmCell<T> {}

impl<T> std::ops::Deref for WasmCell<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {&self.0}
}

impl<T> WasmCell<T> {
    pub const fn new(val: T) -> Self {Self(val)}
}

pub struct MaybeCell<T>(std::cell::RefCell<Option<T>>);

impl<T> MaybeCell<T> {
    #[inline] pub const fn new() -> Self {
        Self(std::cell::RefCell::new(None))
    }

    #[inline] pub fn get<'a>(&'a self) -> JsResult<std::cell::Ref<'a, T>> {
        std::cell::Ref::filter_map(self.0.try_borrow().to_js_result()?,
            |x| x.as_ref()).ok().ok_or_js_error("MaybeCell object not initialised")
    }

    #[inline] pub fn get_mut<'a>(&'a self) -> JsResult<std::cell::RefMut<'a, T>> {
        std::cell::RefMut::filter_map(self.0.try_borrow_mut().to_js_result()?,
            |x| x.as_mut()).ok().ok_or_js_error("MaybeCell object not initialised")
    }

    #[inline] pub fn set(&self, val: T) -> JsResult<()> {
        std::cell::RefMut::map(self.0.try_borrow_mut().to_js_result()?,
            |x| x.insert(val));
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Point {pub x: i32, pub y: i32}

impl From<Point> for [i32; 2] {
    fn from(value: Point) -> Self {[value.x, value.y]}
}

impl std::ops::Add for Point {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self{x: self.x + rhs.x, y: self.y + rhs.y}
    }
}

impl std::ops::AddAssign for Point {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs
    }
}

impl std::ops::Sub for Point {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self{x: self.x - rhs.x, y: self.y - rhs.y}
    }
}

impl std::ops::SubAssign for Point {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs
    }
}

impl std::ops::Neg for Point {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self{x: -self.x, y: -self.y}
    }
}

impl Point {
    pub const ZERO: Self = Self{x:0, y:0};
    pub fn avg(self, p2: Self) -> Self {
        Self{x: ((self.x as i64 + p2.x as i64) / 2) as i32,
            y: ((self.y as i64 + p2.y as i64) / 2) as i32}
    }

    pub fn clamp_x(mut self, min: i32, max: i32) -> Self {
        self.x = self.x.clamp(min, max);
        self
    }

    pub fn clamp_y(mut self, min: i32, max: i32) -> Self {
        self.y = self.y.clamp(min, max);
        self
    }
}

pub trait HitZone: Sized + std::fmt::Debug {
    fn contains(&self, point: Point) -> bool;
    fn     left(&self) -> i32;
    fn      top(&self) -> i32;
    fn    right(&self) -> i32;
    fn   bottom(&self) -> i32;
    fn   center(&self) -> Point;
    fn    shift(self, offset: Point)  -> Self;
    fn     draw(&self, ctx: &web_sys::CanvasRenderingContext2d);

    #[inline] fn   width(&self) -> i32 {self.right() - self.left()}
    #[inline] fn  height(&self) -> i32 {self.bottom() - self.top()}
    #[inline] fn shift_x(self, x: i32) -> Self {self.shift(Point{x, y:0})}
    #[inline] fn shift_y(self, y: i32) -> Self {self.shift(Point{x:0, y})}
}

#[derive(Debug)]
pub struct Rect(Point, Point);

impl HitZone for Rect {
    #[inline] fn contains(&self, point: Point) -> bool {
        self.0.x <= point.x && point.x <= self.1.x
        && self.0.y <= point.y && point.y <= self.1.y
    }

    #[inline] fn   left(&self) -> i32 {self.0.x}
    #[inline] fn    top(&self) -> i32 {self.0.y}
    #[inline] fn  right(&self) -> i32 {self.1.x}
    #[inline] fn bottom(&self) -> i32 {self.1.y}
    #[inline] fn center(&self) -> Point {self.0.avg(self.1)}

    #[inline] fn shift(self, offset: Point) -> Self {
        Self(self.0 + offset, self.1 + offset)
    }

    fn draw(&self, ctx: &web_sys::CanvasRenderingContext2d) {
        ctx.rect(self.left().into(), self.top().into(), self.width().into(), self.height().into())
    }
}

impl Rect {
    #[inline] pub fn square(src: Point, side: i32) -> Self {
        Self(src, src + Point{x: side, y: side})
    }

    #[inline] pub fn square_center(src: Point, mut side: i32) -> Self {
        side /= 2;
        let offset = Point{x: side, y: side};
        Self(src - offset, src + offset)
    }

    #[inline] pub fn to_rhombus(self) -> Rhombus {
        Rhombus::new(self.center(), self.width() / 2, self.height() / 2)
    }
}

#[derive(Debug)]
pub struct Rhombus {
    center: Point,
    half_w: i32, half_h: i32
}

impl HitZone for Rhombus {
    fn contains(&self, point: Point) -> bool {
        let offset = self.center - point;
        offset.x.abs() as f64 / self.half_w as f64 + offset.y.abs() as f64 / self.half_h as f64 <= 1.0
    }

    #[inline] fn   left(&self) -> i32   {self.center.x - self.half_w}
    #[inline] fn    top(&self) -> i32   {self.center.y - self.half_h}
    #[inline] fn  right(&self) -> i32   {self.center.x + self.half_w}
    #[inline] fn bottom(&self) -> i32   {self.center.y + self.half_h}
    #[inline] fn  width(&self) -> i32   {self.half_w * 2}
    #[inline] fn height(&self) -> i32   {self.half_h * 2}
    #[inline] fn center(&self) -> Point {self.center}

    #[inline] fn shift(mut self, offset: Point) -> Self {
        self.center += offset;
        self
    }

    fn draw(&self, ctx: &web_sys::CanvasRenderingContext2d) {
        ctx.move_to(self.center.x.into(), self.top().into());
        ctx.line_to(self.right().into(), self.center.y.into());
        ctx.line_to(self.center.x.into(), self.bottom().into());
        ctx.line_to(self.left().into(), self.center.y.into());
        ctx.close_path();
    }
}

impl Rhombus {
    #[inline] pub fn new(center: Point, half_w: i32, half_h: i32) -> Self {
        Self{center, half_w, half_h}
    }
}

#[derive(Debug)]
pub struct HorizontalArrow {
    back_center: Point,
    w: i32, half_h: i32,
    is_left: bool
}

impl HitZone for HorizontalArrow {
    fn contains(&self, point: Point) -> bool {
        let offset = self.back_center - point;
        if self.is_left.then_invert(offset.x) > 0 {return false}
        offset.x.abs() as f64 / self.w as f64 + offset.y.abs() as f64 / self.half_h as f64 <= 1.0
    }

    #[inline] fn   left(&self) -> i32 {self.back_center.x - self.w * self.is_left as i32}
    #[inline] fn    top(&self) -> i32 {self.back_center.y - self.half_h}
    #[inline] fn  right(&self) -> i32 {self.back_center.x + self.w * !self.is_left as i32}
    #[inline] fn bottom(&self) -> i32 {self.back_center.y + self.half_h}
    #[inline] fn center(&self) -> Point {self.back_center}

    #[inline] fn shift(mut self, offset: Point) -> Self {
        self.back_center += offset;
        self
    }

    fn draw(&self, ctx: &web_sys::CanvasRenderingContext2d) {
        ctx.move_to(self.back_center.x.into(), self.top().into());
        ctx.line_to((self.back_center.x + self.is_left.then_invert(self.w)).into(),
            self.back_center.y.into());
        ctx.line_to(self.back_center.x.into(), self.bottom().into());
        ctx.close_path();
    }
}

impl HorizontalArrow {
    #[inline] pub fn new(back_center: Point, w: i32, half_h: i32, is_left: bool) -> Self {
        Self{back_center, w, half_h, is_left}
    }
}
