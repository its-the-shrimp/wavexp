use std::{ptr, fmt::{Debug, Formatter, self, Display}, ops::{Neg, SubAssign, Sub, AddAssign, Add, Deref}, cell::{RefMut, Ref, RefCell}, iter::successors, error::Error, collections::TryReserveError};
use js_sys::{Object as JsObject, Error as JsError};
use wasm_bindgen::{JsCast, JsValue, throw_val};
use web_sys::{Document as HtmlDocument, Window as HtmlWindow, CanvasRenderingContext2d, HtmlCanvasElement};
use crate::MainCmd;

pub trait Check: Sized {
	#[inline] fn check<F>(self, f: impl FnOnce(&Self) -> bool) -> Result<Self, Self> {
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
	#[inline] fn tee(self, f: impl FnOnce(&Self)) -> Self {
        f(&self); self
	}

    #[inline] fn js_log<'a>(self, label: &'a str) -> Self 
    where Self: Debug {
        js_log!("{}{:?}", label, &self); self
    }
}
impl<T> Tee for T {}

pub trait Pipe: Sized {
	#[inline]
	fn pipe<T>(self, f: impl FnOnce(Self) -> T) -> T { f(self) }
}
impl<T> Pipe for T {}

pub trait BoolExt {
	fn choose<T: Sized>(self, on_true: T, on_false: T) -> T;
    fn choose_with<T: Sized>(self, on_true: impl FnOnce() -> T, on_false: impl FnOnce() -> T) -> T;
    fn then_negate<T: Neg<Output=T>>(self, val: T) -> T;
    fn then_try<T, E>(self, f: impl FnOnce() -> Result<T, E>) -> Result<Option<T>, E>;
}

impl BoolExt for bool {
    #[inline] fn choose<T: Sized>(self, on_true: T, on_false: T) -> T {
        if self {on_true} else {on_false}
    }

    #[inline] fn choose_with<T: Sized>(self, on_true: impl FnOnce() -> T, on_false: impl FnOnce() -> T) -> T {
        if self {on_true()} else {on_false()}
    }

    #[inline] fn then_negate<T: Neg<Output=T>>(self, val: T) -> T {
        if self {-val} else {val}
    }

    #[inline] fn then_try<T, E>(self, f: impl FnOnce() -> Result<T, E>) -> Result<Option<T>, E> {
        self.then(f).transpose()
    }
}
/*
pub trait ArrayExt<T, const M: usize> {
    fn concat<const N: usize>(self, other: [T; N]) -> [T; N + M];
    fn split_first(self) -> (T, [T; M - 1]);
}

impl<T, const M: usize> ArrayExt<T, M> for [T; M] {
    #[inline] fn concat<const N: usize>(self, other: [T; N]) -> [T; N + M] {
        let mut res = std::mem::MaybeUninit::<[T; N + M]>::uninit();
        unsafe {
            let res_ptr = res.as_mut_ptr() as *mut T;
            std::ptr::copy_nonoverlapping(self.as_ptr(), res_ptr, M);
            std::ptr::copy_nonoverlapping(other.as_ptr(), res_ptr.add(M), N);
            res.assume_init()
        }
    }

    #[inline] fn split_first(self) -> (T, [T; M - 1]) {
        let mut res = std::mem::MaybeUninit::<[T; M - 1]>::uninit();
        unsafe {
            let (first, others) = self.as_slice().split_first().unwrap_unchecked();
            std::ptr::copy_nonoverlapping(others.as_ptr(), res.as_mut_ptr() as *mut T, M - 1);
            ((first as *const T).read(), res.assume_init())
        }
    }
}
*/
#[allow(non_camel_case_types)]
#[allow(dead_code)]
pub mod js_types {
    use js_sys::{Number as JsNumber, JsString, Boolean as JsBoolean};
	pub type bool = JsBoolean;
	pub type number = JsNumber;
	pub type str = JsString;
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

pub use web_sys::console::log_1;
#[macro_export]
macro_rules! js_log {
	($arg:literal) => {
		$crate::utils::log_1(&$arg.to_owned().into())
	};
	($f:literal, $($arg:expr),*) => {
		$crate::utils::log_1(&format!($f, $($arg),*).into())
	}
}
pub use js_log;

#[macro_export]
macro_rules! js_try {
    (type = $r:ty : $($s:tt)*) => {
        {let x: crate::utils::JsResult<$r> = try {
            $($s)*
        }; x}
    };

    ($($s:tt)*) => {
        {let x: crate::utils::JsResult<_> = try {
            $($s)*
        }; x}
    };
}
pub use js_try;

pub fn window() -> HtmlWindow {
	unsafe {web_sys::window().unwrap_unchecked()}
}

pub fn document() -> HtmlDocument {
	unsafe {web_sys::window().unwrap_unchecked().document().unwrap_unchecked()}
}

fn to_error_with_msg(err: JsValue, msg: &str) -> JsValue {
    let s = format!("error while {}: {}", msg, 
        match err.dyn_into::<JsError>() {
            Ok(val) => val.message().into(),
            Err(val) => JsObject::from(val).to_string()});
    JsError::new(&s).into()
}

pub type JsResult<T> = Result<T, JsValue>;

pub trait ResultToJsResult<T, E> {
    fn to_js_result(self) -> JsResult<T> where E: Display;
}

pub trait OptionToJsResult<T> {
    fn to_js_result_with(self, f: impl FnOnce() -> String) -> JsResult<T>;
    fn to_js_result(self, msg: &str) -> JsResult<T>;
}

pub trait JsResultUtils<T>: Sized {
    fn explain_err_with(self, f: impl FnOnce() -> String) -> Self;
    fn explain_err(self, msg: &str) -> Self;
    fn expect_throw_with(self, f: impl FnOnce() -> String) -> T;
	fn expect_throw(self, msg: &str) -> T;
    fn report_err_with(self, f: impl FnOnce() -> String) -> Self;
	fn report_err(self, msg: &str) -> Self;
}

impl<T, E> ResultToJsResult<T, E> for Result<T, E> {
    fn to_js_result(self) -> JsResult<T> where E: Display {
        self.map_err(|e| e.to_string().into())
    }
}

impl<T> OptionToJsResult<T> for Option<T> {
    #[inline] fn to_js_result_with(self, f: impl FnOnce() -> String) -> JsResult<T> {
        self.ok_or_else(|| JsError::new(&f()).into())
    }

    #[inline] fn to_js_result(self, msg: &str) -> JsResult<T> {
        self.ok_or_else(|| JsError::new(msg).into())
    }
}

impl<T> JsResultUtils<T> for JsResult<T> {
    #[inline] fn explain_err_with(self, f: impl FnOnce() -> String) -> Self {
        self.map_err(|err| to_error_with_msg(err, &f()))
    }
    #[inline] fn explain_err(self, msg: &str) -> Self {
        self.map_err(|err| to_error_with_msg(err, msg))
    }

    #[inline] fn expect_throw_with(self, f: impl FnOnce() -> String) -> T {
		match self {
			Ok(val)  => val,
			Err(err) => throw_val(to_error_with_msg(err, &f()))}
	}

    #[inline] fn expect_throw(self, msg: &str) -> T {
        match self {
            Ok(val) => val,
            Err(err) => throw_val(to_error_with_msg(err, msg))}
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
    fn get_2d_context(&self) -> JsResult<CanvasRenderingContext2d>;
    fn sync(&self);
}

impl HtmlCanvasExt for HtmlCanvasElement {
    fn get_2d_context(&self) -> JsResult<CanvasRenderingContext2d> {
        Ok(self.get_context("2d")?
            .to_js_result("the element has no rendering context")?
            .unchecked_into::<CanvasRenderingContext2d>())
    }

    fn sync(&self) {
        self.set_width(300);
        self.set_height((self.client_height() as f64 / self.client_width() as f64 * 300.0) as u32);
    }
}

pub trait HtmlDocumentExt {
    fn element_dyn_into<T: JsCast>(&self, id: &str) -> JsResult<T>;
}

impl HtmlDocumentExt for HtmlDocument {
    fn element_dyn_into<T: JsCast>(&self, id: &str) -> JsResult<T> {
        Ok(self.get_element_by_id(id).to_js_result_with(|| format!("element #{} not found", id))?
            .dyn_into::<T>().ok().to_js_result_with(|| format!("element #{} is not of type `{}`", id, std::any::type_name::<T>()))?)
    }
}

pub enum GetVarError {
    OutOfBounds(usize),
    Overlap(usize)
}

impl Display for GetVarError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GetVarError::OutOfBounds(x) => write!(f, "index #{} is out of bounds", x),
            GetVarError::Overlap(x) => write!(f, "index #{} appeared more than once", x)}
    }
}

pub trait SliceExt<T> {
    fn get_saturating<'a>(&'a self, id: usize) -> &'a T;
    fn get_saturating_mut<'a>(&'a mut self, id: usize) -> &'a mut T;
    // the format of the error message would be: <prefix><id><postifx>
    fn get_or_js_error<'a>(&'a self, id: usize, prefix: &str, postfix: &str) -> JsResult<&'a T>;
    fn get_mut_or_js_error<'a>(&'a mut self, id: usize, prefix: &str, postfix: &str) -> JsResult<&'a mut T>;
    fn get_var<'a>(&'a self, ids: &[usize]) -> Result<Vec<&'a T>, GetVarError>;
    fn get_var_mut<'a>(&'a mut self, ids: &[usize]) -> Result<Vec<&'a mut T>, GetVarError>;
}

impl<T> SliceExt<T> for [T] {
    #[inline] fn get_saturating<'a>(&'a self, id: usize) -> &'a T {
        unsafe{self.get_unchecked(id.min(self.len() - 1))}
    }

    #[inline] fn get_saturating_mut<'a>(&'a mut self, id: usize) -> &'a mut T {
        unsafe{self.get_unchecked_mut(id.min(self.len() - 1))}
    }

    #[inline] fn get_or_js_error<'a>(&'a self, id: usize, prefix: &str, postfix: &str) -> JsResult<&'a T> {
        self.get(id).to_js_result_with(|| format!("{}{}{}", prefix, id, postfix))
    }

    #[inline] fn get_mut_or_js_error<'a>(&'a mut self, id: usize, prefix: &str, postfix: &str) -> JsResult<&'a mut T> {
        self.get_mut (id).to_js_result_with(|| format!("{}{}{}", prefix, id, postfix))
    }

    #[inline] fn get_var<'a>(&'a self, ids: &[usize]) -> Result<Vec<&'a T>, GetVarError> {
        let len = self.len();
        for (id, rest) in successors(ids.split_first(), |x| x.1.split_first()) {
            if *id >= len {return Err(GetVarError::OutOfBounds(*id))}
            if rest.contains(id) {return Err(GetVarError::Overlap(*id))}
        }
        Ok(unsafe { // at this point, `ids` is guaranteed to contain unique valid indices into `self`
            let base = self.as_ptr();
            ids.iter().map(|x| &*base.add(*x)).collect::<Vec<_>>()
        })
    }

    #[inline] fn get_var_mut<'a>(&'a mut self, ids: &[usize]) -> Result<Vec<&'a mut T>, GetVarError> {
        let len = self.len();
        for (id, rest) in successors(ids.split_first(), |x| x.1.split_first()) {
            if *id >= len {return Err(GetVarError::OutOfBounds(*id))}
            if rest.contains(id) {return Err(GetVarError::Overlap(*id))}
        }
        Ok(unsafe { // at this point, `ids` is guaranteed to contain unique valid indices into `self`
            let base = self.as_mut_ptr();
            ids.iter().map(|x| &mut*base.add(*x)).collect::<Vec<_>>()
        })
    }
}

#[derive(Debug)]
pub struct SwapRemoveError {
    index: usize,
    len: usize
}

impl Display for SwapRemoveError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "swap_remove index (is {}) should be < len (is {})", self.index, self.len)
    }
}

impl Error for SwapRemoveError {}

#[derive(Debug)]
pub enum InsertError {
    Index{index: usize, len: usize},
    Alloc(TryReserveError)
}

impl Display for InsertError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Index{index, len} => write!(f, "insertion index (is {}) should be <= len (is {})", index, len),
            Self::Alloc(err) => Debug::fmt(err, f)
        }
    }
}

impl Error for InsertError {}

pub trait VecExt<T> {
    fn try_swap_remove(&mut self, index: usize) -> Result<T, SwapRemoveError>;
    fn try_insert<'a>(&'a mut self, index: usize, element: T) -> Result<&'a mut T, InsertError>;
}

impl<T> VecExt<T> for Vec<T> {
    fn try_swap_remove(&mut self, index: usize) -> Result<T, SwapRemoveError> {
        let len = self.len();
        if index >= len {
            return Err(SwapRemoveError{index, len});
        }
        unsafe {
            let value = ptr::read(self.as_ptr().add(index));
            let base_ptr = self.as_mut_ptr();
            ptr::copy(base_ptr.add(len - 1), base_ptr.add(index), 1);
            self.set_len(len - 1);
            Ok(value)
        }
    }

    fn try_insert<'a>(&'a mut self, index: usize, element: T) -> Result<&'a mut T, InsertError> {
        let len = self.len();
        if len == self.capacity() {
            self.try_reserve(1).map_err(InsertError::Alloc)?;
        }
        unsafe {
            let p = self.as_mut_ptr().add(index);
            if index < len {
                ptr::copy(p, p.add(1), len - index);
            } else if index > len {
                return Err(InsertError::Index{index, len})
            }
            ptr::write(p, element);
            self.set_len(len + 1);
            Ok(&mut *p)
        }
    }
}

// this exists to circumvent a limiatation on static variables that Rust imposes, which prevents
// them from containing types that don't implement `Sync`. On any other architecture this
// limitation makes sense, but in Webassembly, which doesn't support threading, this limitation is meaningless.
pub struct WasmCell<T>(T);

unsafe impl<T> Sync for WasmCell<T> {}

impl<T> Deref for WasmCell<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {&self.0}
}

impl<T> WasmCell<T> {
    pub const fn new(val: T) -> Self {Self(val)}
}

pub struct MaybeCell<T>(RefCell<Option<T>>);

impl<T> MaybeCell<T> {
    #[inline] pub const fn new() -> Self {
        Self(RefCell::new(None))
    }

    #[inline] pub fn get<'a>(&'a self) -> JsResult<Ref<'a, T>> {
        Ref::filter_map(self.0.try_borrow().to_js_result()?,
            |x| x.as_ref()).ok().to_js_result("MaybeCell object not initialised")
    }

    #[inline] pub fn get_mut<'a>(&'a self) -> JsResult<RefMut<'a, T>> {
        RefMut::filter_map(self.0.try_borrow_mut().to_js_result()?,
            |x| x.as_mut()).ok().to_js_result("MaybeCell object not initialised")
    }

    #[inline] pub fn set(&self, val: T) -> JsResult<()> {
        RefMut::map(self.0.try_borrow_mut().to_js_result()?,
            |x| x.insert(val));
        Ok(())
    }

    #[inline] pub fn maybe_set(&self, val: Option<T>) -> JsResult<()> {
        let mut r = self.0.try_borrow_mut().to_js_result()?;
        *r = val;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Point {pub x: i32, pub y: i32}

impl From<Point> for [i32; 2] {
    fn from(value: Point) -> Self {[value.x, value.y]}
}

impl Add for Point {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self{x: self.x + rhs.x, y: self.y + rhs.y}
    }
}

impl AddAssign for Point {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs
    }
}

impl Sub for Point {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self{x: self.x - rhs.x, y: self.y - rhs.y}
    }
}

impl SubAssign for Point {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs
    }
}

impl Neg for Point {
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

pub trait HitZone: Sized + Debug {
    fn contains(&self, point: Point) -> bool;
    fn     left(&self) -> i32;
    fn      top(&self) -> i32;
    fn    right(&self) -> i32;
    fn   bottom(&self) -> i32;
    fn   center(&self) -> Point;
    fn    shift(self, offset: Point)  -> Self;
    fn     draw(&self, ctx: &CanvasRenderingContext2d);

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

    fn draw(&self, ctx: &CanvasRenderingContext2d) {
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

    fn draw(&self, ctx: &CanvasRenderingContext2d) {
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
        if self.is_left.then_negate(offset.x) > 0 {return false}
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

    fn draw(&self, ctx: &CanvasRenderingContext2d) {
        ctx.move_to(self.back_center.x.into(), self.top().into());
        ctx.line_to((self.back_center.x + self.is_left.then_negate(self.w)).into(),
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
