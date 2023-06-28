use std::{
    ptr,
    mem::{swap, take},
    fmt::{self, Debug, Formatter, Display},
    ops::{Neg, SubAssign, Sub, AddAssign, Add, Deref, RangeBounds, Mul, MulAssign, DivAssign, Div, Rem, RemAssign, Range},
    iter::successors,
    error::Error,
    collections::TryReserveError,
    cmp::Ordering,
    any::type_name,
    num::FpCategory};
use js_sys::{Object as JsObject, Error as JsError};
use wasm_bindgen::{JsCast, JsValue, throw_val};
use web_sys::{Document as HtmlDocument, Window as HtmlWindow, CanvasRenderingContext2d, HtmlCanvasElement, Element, console::warn_1, HtmlElement};

pub trait Check: Sized {
	#[inline] fn check(self, f: impl FnOnce(&Self) -> bool) -> Result<Self, Self> {
		if f(&self) {Ok(self)} else {Err(self)}
	}

    #[inline] fn check_in<R>(self, range: R) -> Result<Self, Self>
	where Self: PartialOrd, R: RangeBounds<Self> {
		if range.contains(&self) {Ok(self)} else {Err(self)}
	}

	#[inline] fn check_not_in<R>(self, range: R) -> Result<Self, Self>
	where Self: PartialOrd, R: RangeBounds<Self> {
		if !range.contains(&self) {Ok(self)} else {Err(self)}
	}
}
impl<T> Check for T {}

pub trait Tee: Sized {
	#[inline] fn tee(self, f: impl FnOnce(&Self)) -> Self {
        f(&self); self
	}

    #[inline] fn js_log(self, label: &str) -> Self 
    where Self: Debug {
        js_log!("{}{:?}", label, &self); self
    }
}
impl<T> Tee for T {}

pub trait Pipe: Sized {
	#[inline] fn pipe<T>(self, f: impl FnOnce(Self) -> T) -> T {
        f(self)
    }

    #[inline] fn pipe_if(self, cond: bool, f: impl FnOnce(Self) -> Self) -> Self {
        if cond {f(self)} else {self}
    }
}
impl<T> Pipe for T {}

pub trait BoolExt {
	fn choose<T>(self, on_true: T, on_false: T) -> T;
    fn then_or<T>(self, default: T, f: impl FnOnce() -> T) -> T;
    fn then_or_else<T>(self, default: impl FnOnce() -> T, f: impl FnOnce() -> T) -> T;
    fn then_negate<T: Neg<Output=T>>(self, val: T) -> T;
    fn then_try<T, E>(self, f: impl FnOnce() -> Result<T, E>) -> Result<Option<T>, E>;
    fn and_then<T>(self, f: impl FnOnce() -> Option<T>) -> Option<T>;
    fn toggle(&mut self) -> Self;
}

impl BoolExt for bool {
    #[inline] fn choose<T>(self, on_true: T, on_false: T) -> T {
        if self {on_true} else {on_false}
    }

    #[inline] fn then_or<T>(self, default: T, f: impl FnOnce() -> T) -> T {
        if self {f()} else {default}
    }

    #[inline] fn then_or_else<T>(self, default: impl FnOnce() -> T, f: impl FnOnce() -> T) -> T {
        if self {f()} else {default()}
    }

    #[inline] fn then_negate<T: Neg<Output=T>>(self, val: T) -> T {
        if self {-val} else {val}
    }

    #[inline] fn then_try<T, E>(self, f: impl FnOnce() -> Result<T, E>) -> Result<Option<T>, E> {
        self.then(f).transpose()
    }

    #[inline] fn and_then<T>(self, f: impl FnOnce() -> Option<T>) -> Option<T> {
        if self {f()} else {None}
    }

    #[inline] fn toggle(&mut self) -> Self {
        let res = *self;
        *self = !*self;
        res
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

#[allow(non_camel_case_types)]
#[allow(dead_code)]
pub mod js_types {
    use js_sys::{Number as JsNumber, JsString, Boolean as JsBoolean};
	pub type bool = JsBoolean;
	pub type number = JsNumber;
	pub type str = JsString;
}

#[macro_export]
macro_rules! js_array {
    ($($t:ident $v:expr),*) => {{
        let res = $crate::js_sys::Array::new();
        $( res.push(&*$crate::utils::js_types::$t::from($v)); )*
        $crate::wasm_bindgen::JsValue::from(res)
    }};
}
pub use js_array;

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
        $crate::utils::log_1(&format!($arg).into())
	};
	($f:literal, $($arg:expr),*) => {
		$crate::utils::log_1(&format!($f, $($arg),*).into())
	}
}
pub use js_log;

#[macro_export]
macro_rules! js_try {
    (type = $r:ty : $($s:tt)*) => {
        {let x: $crate::utils::JsResult<$r> = try {
            $($s)*
        }; x}
    };

    ($($s:tt)*) => {
        {let x: $crate::utils::JsResult<_> = try {
            $($s)*
        }; x}
    };
}
pub use js_try;

#[macro_export]
macro_rules! js_assert {
    ($($s:tt)+) => {
        if !$($s)+ {
            Err(js_sys::Error::new(stringify!($($s)+)).into()).add_loc(loc!())
        } else {
            Ok(())
        }
    };
}
pub use js_assert;

#[macro_export]
macro_rules! eval_once {
    ($t:ty : $e:expr) => {{
        static RES: $crate::utils::WasmCell<std::cell::OnceCell<$t>> = $crate::utils::WasmCell::new(std::cell::OnceCell::new());
        RES.get_or_init(|| $e)
    }};
}

pub fn window() -> HtmlWindow {
	unsafe {web_sys::window().unwrap_unchecked()}
}

pub fn document() -> HtmlDocument {
	unsafe {web_sys::window().unwrap_unchecked().document().unwrap_unchecked()}
}

pub fn report_err(err: JsValue) {
    warn_1(&err);
    document().element_dyn_into::<HtmlElement>("error-sign").unwrap_throw(loc!())
        .set_hidden(false);
}

fn to_error_with_msg(err: JsValue, msg: &str) -> JsValue {
    let s = format!("{}\n{}", msg, 
        match err.dyn_into::<JsError>() {
            Ok(val) => val.message(),
            Err(val) => JsObject::from(val).to_string()});
    JsError::new(&s).into()
}

/// returns a Result for easier handling in `try` blocks
/// the second argument is recommended to be passed from the `loc!` macro
#[inline] pub fn js_error(msg: impl AsRef<str>, loc: (&str, u32, u32)) -> JsResult<!> {
    Err(JsError::new(msg.as_ref()).into()).add_loc(loc)
}

pub type JsResult<T> = Result<T, JsValue>;

pub trait ResultToJsResult<T, E> {
    fn to_js_result(self, loc: (&str, u32, u32)) -> JsResult<T> where E: Display;
    fn to_js_result_with<R: AsRef<str>>(self, f: impl FnOnce(E) -> R, loc: (&str, u32, u32)) -> JsResult<T>;
}

pub trait OptionExt<T> {
    fn to_js_result(self, loc: (&str, u32, u32)) -> JsResult<T>;
    fn report_err(self, loc: (&str, u32, u32)) -> Self;
    fn unwrap_throw(self, loc: (&str, u32, u32)) -> T;
    fn map_or_default<U: Default>(self, f: impl FnOnce(T) -> U) -> U;
    fn choose<U>(&self, on_some: U, on_none: U) -> U;
    fn drop(self) -> Option<()>;
}

pub trait JsResultUtils<T>: Sized {
    fn explain_err_with(self, f: impl FnOnce() -> String) -> Self;
    fn explain_err(self, msg: &str) -> Self;
    fn expect_throw_with(self, f: impl FnOnce() -> String) -> T;
	fn expect_throw(self, msg: &str) -> T;
    fn unwrap_throw(self, loc: (&str, u32, u32)) -> T;
	fn report_err(self, loc: (&str, u32, u32)) -> Self;
    /// best used with the `loc!` macro
    fn add_loc(self, loc: (&str, u32, u32)) -> Self;
}

#[macro_export]
macro_rules! loc {
    () => {(file!(), line!(), column!())};
}
pub use loc;

impl<T, E> ResultToJsResult<T, E> for Result<T, E> {
    #[inline] fn to_js_result(self, loc: (&str, u32, u32)) -> JsResult<T> where E: Display {
        self.map_err(|e| e.to_string().into()).add_loc(loc)
    }

    #[inline] fn to_js_result_with<R: AsRef<str>>(self, f: impl FnOnce(E) -> R, loc: (&str, u32, u32)) -> JsResult<T> {
        self.map_err(|e| JsError::new(f(e).as_ref()).into()).add_loc(loc)
    }
}

impl<T> OptionExt<T> for Option<T> {
    #[inline] fn to_js_result(self, loc: (&str, u32, u32)) -> JsResult<T> {
        self.ok_or_else(|| JsError::new("`Option` contained the `None` value").into())
            .add_loc(loc)
    }

    #[inline] fn report_err(self, loc: (&str, u32, u32)) -> Self {
        if self.is_none() {
            report_err(js_error("`Option` contained the `None` value", loc).into_err())
        }
        self
    }

    #[inline] fn unwrap_throw(self, loc: (&str, u32, u32)) -> T {
        self.to_js_result(loc).unwrap_or_else(|x| throw_val(x))
    }

    #[inline] fn map_or_default<U: Default>(self, f: impl FnOnce(T) -> U) -> U {
        match self {Some(x) => f(x), None => U::default()}
    }

    #[inline] fn choose<U>(&self, on_some: U, on_none: U) -> U {
        if self.is_some() {on_some} else {on_none}
    }

    #[allow(clippy::manual_map)]
    #[inline] fn drop(self) -> Option<()> {
        match self {Some(_) => Some(()), None => None}
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

    #[inline] fn unwrap_throw(self, loc: (&str, u32, u32)) -> T {
        self.add_loc(loc).unwrap_or_else(|x| throw_val(x))
    }

    #[inline]
    fn report_err(mut self, loc: (&str, u32, u32)) -> Self {
        self = self.add_loc(loc);
        if let Err(err) = &self {
            report_err(err.clone())
        }
        self
    }

    #[inline] fn add_loc(self, loc: (&str, u32, u32)) -> Self {
        self.map_err(|x| to_error_with_msg(x, &format!("in {}:{}:{}", loc.0, loc.1, loc.2)))
    }
}

pub trait HtmlCanvasExt {
    fn get_2d_context(&self, loc: (&str, u32, u32)) -> JsResult<CanvasRenderingContext2d>;
    fn rect(&self) -> Rect;
    fn size(&self) -> [u32; 2];
    fn sync(&self);
}

impl HtmlCanvasExt for HtmlCanvasElement {
    fn get_2d_context(&self, loc: (&str, u32, u32)) -> JsResult<CanvasRenderingContext2d> {
        Ok(self.get_context("2d").add_loc(loc!()).add_loc(loc)?
            .to_js_result(loc!()).add_loc(loc)?
            .unchecked_into::<CanvasRenderingContext2d>())
    }

    fn rect(&self) -> Rect {
        Rect(Point::ZERO, Point{x: self.width() as i32, y: self.height() as i32})
    }

    fn size(&self) -> [u32; 2] {[self.width(), self.height()]}

    fn sync(&self) {
        self.set_height((self.client_height() as f64 / self.client_width() as f64 * self.width() as f64) as u32);
    }
}

pub trait HtmlDocumentExt {
    fn element_dyn_into<T: JsCast>(&self, id: &str) -> JsResult<T>;
}

impl HtmlDocumentExt for HtmlDocument {
    fn element_dyn_into<T: JsCast>(&self, id: &str) -> JsResult<T> {
        self.get_element_by_id(id).to_js_result(loc!())?
            .dyn_into::<T>()
            .to_js_result_with(|_| format!("element #{} is not of type `{}`", id, type_name::<T>()), loc!())
    }
}

pub trait HtmlElementExt {
    fn client_rect(&self) -> Rect;
    fn client_size(&self) -> [i32; 2];
}

impl HtmlElementExt for Element {
    fn client_rect(&self) -> Rect {
        Rect(Point::ZERO, Point{x: self.client_width(), y: self.client_height()})
    }

    fn client_size(&self) -> [i32; 2] {
        [self.client_width(), self.client_height()]
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum GetVarError {
    OutOfBounds(usize, usize),
    Overlap(usize)
}

impl Display for GetVarError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GetVarError::OutOfBounds(x, len) =>
                write!(f, "index #{x} is out of bounds for a slice of length {len}"),
            GetVarError::Overlap(x) =>
                write!(f, "index #{x} appeared more than once")}
    }
}

impl Error for GetVarError {}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ReorderError {
    index: usize,
    len: usize
}

impl Display for ReorderError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "SliceExt::reorder: index #{} is out of bounds for a slice of length {}",
            self.index, self.len)
    }
}

impl Error for ReorderError {}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct SetSortedError {
    index: usize,
    len: usize
}

impl Display for SetSortedError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "SliceExt::set_sorted: index #{} is out of bounds for a slice of length {}",
            self.index, self.len)
    }
}

impl Error for SetSortedError {}

pub trait SliceExt<T> {
    fn get_saturating(&self, id: usize) -> &T;
    fn get_saturating_mut(&mut self, id: usize) -> &mut T;
    fn get_wrapping(&self, id: usize) -> &T;
    fn get_wrapping_mut(&mut self, id: usize) -> &mut T;
    fn get_var<'a>(&'a self, ids: &[usize]) -> Result<Vec<&'a T>, GetVarError>;
    fn get_var_mut<'a>(&'a mut self, ids: &[usize]) -> Result<Vec<&'a mut T>, GetVarError>;
    unsafe fn reorder_unchecked(&mut self, index: usize) -> usize
        where T: Ord;
    // unsafe fn reorder_unchecked_by<F>(&mut self, index: usize, f: F) -> usize
    //  where F: FnMut(&T, &T) -> Ordering
    unsafe fn reorder_unchecked_by_key<K, F>(&mut self, index: usize, f: F) -> usize
        where F: FnMut(&T) -> K, K: Ord;
    fn reorder(&mut self, index: usize) -> Result<usize, ReorderError>
        where T: Ord;
    // fn reorder_by<F>(&mut self, index: usize, f: F) -> Result<usize, ReorderError>
    //  where F: FnMut(&T, &T) -> Ordering
    // fn reorder_by_key<K, F>(&mut self, index: usize, f: F) -> Result<usize, ReorderError>
    //  where F: FnMut(&T) -> K, K: Ord
    fn set_sorted(&mut self, index: usize, value: T) -> Result<usize, SetSortedError>
        where T: Ord;
    // fn set_sorted_by<F>(&mut self, index: usize, value: T, f: F) -> Result<usize, SetSortedError>
    //  where F: FnMut(&T, &T) -> Ordering
    // fn set_sorted_by_key<K, F>(&mut self, index: usize, value: T, f: F) -> Result<usize, SetSortedError>
    //  where F: FnMut(&T) -> K, K: Ord
}

impl<T> SliceExt<T> for [T] {
    #[inline] fn get_saturating(&self, id: usize) -> &T {
        unsafe{self.get_unchecked(id.min(self.len() - 1))}
    }

    #[inline] fn get_saturating_mut(&mut self, id: usize) -> &mut T {
        unsafe{self.get_unchecked_mut(id.min(self.len() - 1))}
    }

    #[inline] fn get_wrapping(&self, id: usize) -> &T {
        unsafe{self.get_unchecked(id % self.len())}
    }

    #[inline] fn get_wrapping_mut(&mut self, id: usize) -> &mut T {
        unsafe{self.get_unchecked_mut(id % self.len())}
    }


    #[inline] fn get_var<'a>(&'a self, ids: &[usize]) -> Result<Vec<&'a T>, GetVarError> {
        let len = self.len();
        for (id, rest) in successors(ids.split_first(), |x| x.1.split_first()) {
            if *id >= len {return Err(GetVarError::OutOfBounds(*id, len))}
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
            if *id >= len {return Err(GetVarError::OutOfBounds(*id, len))}
            if rest.contains(id) {return Err(GetVarError::Overlap(*id))}
        }
        Ok(unsafe { // at this point, `ids` is guaranteed to contain unique valid indices into `self`
            let base = self.as_mut_ptr();
            ids.iter().map(|x| &mut*base.add(*x)).collect::<Vec<_>>()
        })
    }

    unsafe fn reorder_unchecked(&mut self, index: usize) -> usize where T: Ord {
        let element = self.get_unchecked(index);
        let (new, should_move) = self.get_unchecked(..index).binary_search(element)
            .map_or_else(|x| (x, x != index), |x| (x, x < index - 1));
        if should_move {
            self.get_unchecked_mut(new..=index).rotate_right(1);
            return new}
        let new = self.get_unchecked(index+1..).binary_search(element)
            .unwrap_or_else(|x| x) + index;
        if new > index {
            self.get_unchecked_mut(index..=new).rotate_left(1);
        }
        new
    }

    unsafe fn reorder_unchecked_by_key<K, F>(&mut self, index: usize, mut f: F) -> usize
    where F: FnMut(&T) -> K, K: Ord {
        let key = f(self.get_unchecked(index));
        let (new, should_move) = self.get_unchecked(..index).binary_search_by_key(&key, &mut f)
            .map_or_else(|x| (x, x != index), |x| (x, x < index - 1));
        if should_move {
            self.get_unchecked_mut(new..=index).rotate_right(1);
            return new}
        let new = self.get_unchecked(index+1..).binary_search_by_key(&key, &mut f)
            .unwrap_or_else(|x| x) + index;
        if new > index {
            self.get_unchecked_mut(index..=new).rotate_left(1);
        }
        new
    }

    #[inline] fn reorder(&mut self, index: usize) -> Result<usize, ReorderError> where T: Ord {
        let len = self.len();
        if index >= len {
            return Err(ReorderError{index, len});
        }
        Ok(unsafe{self.reorder_unchecked(index)})
    }

    #[inline] fn set_sorted(&mut self, index: usize, value: T) -> Result<usize, SetSortedError> where T: Ord {
        let len = self.len();
        if index >= len {
            return Err(SetSortedError{index, len});
        }
        Ok(unsafe {
            let dst = self.get_unchecked_mut(index);
            let should_reorder = &value != dst;
            *dst = value;
            if should_reorder {self.reorder_unchecked(index)} else {index}
        })
    }
}

#[test] fn slice_get_var() {
    let x = [1, 2, 4, 8, 16, 32, 64];
    assert_eq!(x.get_var(&[1, 3, 6]), Ok(vec![&2, &8, &64]));
    assert_eq!(x.get_var(&[1, 25]), Err(GetVarError::OutOfBounds(25, 7)));
    assert_eq!(x.get_var(&[1, 4, 5, 1]), Err(GetVarError::Overlap(1)));
}

#[test] fn slice_get_var_mut() {
    let mut x = [1, 2, 4, 8, 16, 32, 64];
    assert_eq!(x.get_var_mut(&[1, 3, 6]), Ok(vec![&mut 2, &mut 8, &mut 64]));
    assert_eq!(x.get_var_mut(&[1, 25]), Err(GetVarError::OutOfBounds(25, 7)));
    assert_eq!(x.get_var_mut(&[1, 4, 5, 1]), Err(GetVarError::Overlap(1)));
}

#[test] fn slice_reorder() {
    let mut x = [1, 2, 4, 8, 16, 32, 64];
    let old_x = x;
    assert_eq!(x.reorder(3), Ok(3));
    assert_eq!(x, old_x);
    x[1] = 17;
    assert_eq!(x.reorder(1), Ok(4));
    // [1, 2, 4, 8, 16, 32, 64] > [1, 4, 8, 16, 17, 32, 64]
    x[5] = 3;
    assert_eq!(x.reorder(5), Ok(1));
    // [1, 4, 8, 16, 17, 32, 64] > [1, 3, 4, 8, 16, 17, 64]
    let old_x = x;
    assert_eq!(x.reorder(69), Err(ReorderError{index: 69, len: 7}));
    assert_eq!(x, old_x);
    x[2] = 3;
    assert_eq!(x.reorder(2), Ok(2));
}

#[derive(Debug, PartialEq, Eq)]
pub struct RemoveError {
    index: usize,
    len: usize
}

impl Display for RemoveError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "removal index (is {}) should be < len (is {})", self.index, self.len)
    }
}

impl Error for RemoveError {}

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
    fn try_remove(&mut self, index: usize) -> Result<T, RemoveError>;
    unsafe fn remove_unchecked(&mut self, index: usize) -> T;
    fn try_swap_remove(&mut self, index: usize) -> Result<T, RemoveError>;
    fn try_insert(&mut self, index: usize, element: T) -> Result<&mut T, InsertError>;
    fn push_unique(&mut self, value: T, f: impl Fn(&T, &T) -> bool) -> bool;
    fn push_sorted(&mut self, value: T) -> usize where T: Ord;
    fn push_sorted_by(&mut self, value: T, f: impl Fn(&T, &T) -> Ordering) -> usize;
    fn push_sorted_by_key<K: Ord>(&mut self, value: T, f: impl FnMut(&T) -> K) -> usize;
}

impl<T> VecExt<T> for Vec<T> {
    #[inline] fn try_remove(&mut self, index: usize) -> Result<T, RemoveError> {
        let len = self.len();
        if index >= len {
            return Err(RemoveError{index, len})
        }
        unsafe {
            let ptr = self.as_mut_ptr().add(index);
            let ret = ptr::read(ptr);
            ptr::copy(ptr.add(1), ptr, len - index - 1);
            self.set_len(len - 1);
            Ok(ret)
        }
    }

    #[inline] unsafe fn remove_unchecked(&mut self, index: usize) -> T {
        let len = self.len();
        let ptr = self.as_mut_ptr().add(index);
        let ret = ptr::read(ptr);
        ptr::copy(ptr.add(1), ptr, len - index - 1);
        self.set_len(len - 1);
        ret
    }

    #[inline] fn try_swap_remove(&mut self, index: usize) -> Result<T, RemoveError> {
        let len = self.len();
        if index >= len {
            return Err(RemoveError{index, len});
        }
        unsafe {
            let value = ptr::read(self.as_ptr().add(index));
            let base_ptr = self.as_mut_ptr();
            ptr::copy(base_ptr.add(len - 1), base_ptr.add(index), 1);
            self.set_len(len - 1);
            Ok(value)
        }
    }

    #[inline] fn try_insert(&mut self, index: usize, element: T) -> Result<&mut T, InsertError> {
        let len = self.len();
        if index > len {
            return Err(InsertError::Index{index, len});
        }
        if len == self.capacity() {
            self.try_reserve(1).map_err(InsertError::Alloc)?;
        }
        unsafe {
            let p = self.as_mut_ptr().add(index);
            ptr::copy(p, p.add(1), len - index);
            ptr::write(p, element);
            self.set_len(len + 1);
            Ok(&mut *p)
        }
    }

    fn push_unique(&mut self, value: T, f: impl Fn(&T, &T) -> bool) -> bool {
        if self.iter().any(|x| f(x, &value)) {return false}
        self.push(value);
        true
    }

    fn push_sorted(&mut self, value: T) -> usize where T: Ord {
        let id = self.binary_search(&value).unwrap_or_else(|x| x);
        self.insert(id, value);
        id
    }

    fn push_sorted_by(&mut self, value: T, f: impl Fn(&T, &T) -> Ordering) -> usize {
        let id = self.binary_search_by(|x| f(&value, x)).unwrap_or_else(|x| x);
        self.insert(id, value);
        id
    }

    fn push_sorted_by_key<K: Ord>(&mut self, value: T, mut f: impl FnMut(&T) -> K) -> usize {
        let id = self.binary_search_by_key(&f(&value), f).unwrap_or_else(|x| x);
        self.insert(id, value);
        id
    }
}

pub trait Take: Default {
    /// replaces the value with a default one and returns the previous value
    #[inline] fn take(&mut self) -> Self {take(self)}
}
impl<T: Default> Take for T {}

pub trait RangeExt<T> {
    fn overlap<O>(&self, other: &Range<O>) -> bool
    where O: PartialOrd<T>, T: PartialOrd<O>;
    fn loose_contain<O, I>(&self, item: I, offset: O) -> bool where
    O: Copy,
    I: PartialOrd<T> + Add<O, Output=I> + Sub<O, Output=I> + Copy,
    T: PartialOrd<I>;
    fn fit<R>(&self, item: R) -> R where T: Clone + Into<R> + PartialOrd<R>;
    /// if `value` is outside of `self`, extend `self` just enough for `value` to be inside it
    fn extend<R>(self, value: R) -> Self where T: PartialOrd<R> + From<R>;
    /// turns `x .. y` into `f(x) .. f(y)`
    fn map<R>(self, f: impl FnMut(T) -> R) -> Range<R>;
}

impl<T> RangeExt<T> for Range<T> {
    #[inline] fn overlap<O>(&self, other: &Range<O>) -> bool
    where O: PartialOrd<T>, T: PartialOrd<O> {
        self.contains(&other.start)
            || self.contains(&other.end)
            || other.contains(&self.start)
    }

    #[inline] fn loose_contain<O, I>(&self, item: I, offset: O) -> bool
    where O: Copy,
    I: PartialOrd<T> + Add<O, Output=I> + Sub<O, Output=I> + Copy,
    T: PartialOrd<I> {
        self.overlap(&(item - offset .. item + offset))
    }

    #[inline] fn fit<R>(&self, item: R) -> R
    where T: Clone + Into<R> + PartialOrd<R> {
        if      self.end  <= item {self.end.clone().into()}
        else if self.start > item {self.start.clone().into()}
        else                       {item}
    }

    #[inline] fn extend<R>(self, value: R) -> Self
    where T: PartialOrd<R> + From<R> {
        if      self.start > value {value.into() .. self.end}
        else if self.end  <= value {self.start .. value.into()}
        else {self}
    }

    #[inline] fn map<R>(self, mut f: impl FnMut(T) -> R) -> Range<R> {
        f(self.start) .. f(self.end)
    }
}

#[test]
fn range_overlap() {
    assert!(!(50 .. 55).overlap(&(56 .. 61)));
    assert!(!(50 .. 56).overlap(&(56 .. 61)));
    assert!( (50 .. 57).overlap(&(56 .. 61)));
    assert!( (58 .. 60).overlap(&(56 .. 61)));
    assert!( (56 .. 61).overlap(&(58 .. 60)));
    assert!(!(56 .. 61).overlap(&(61 .. 67)));
}

pub trait LooseEq<O = Self> {
    fn loose_eq(&self, value: Self, off: O) -> bool;
    #[inline] fn loose_ne(&self, value: Self, off: O) -> bool
    where Self: Sized {
        !self.loose_eq(value, off)
    }
}

impl<O: Copy, T: PartialOrd + Add<O, Output=Self> + Sub<O, Output=Self> + Copy> LooseEq<O> for T {
    #[inline] fn loose_eq(&self, value: Self, off: O) -> bool {
        (value - off .. value + off).contains(self)
    }
}

/// function just like `Ord::clamp`, except that instead of panicking when `min > max`,
/// it swaps `min` and `max`
pub fn total_clamp<T: Ord>(x: T, mut min: T, mut max: T) -> T {
    if min > max {swap(&mut min, &mut max)}
    if x < min {min}
    else if x > max {max}
    else {x}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Point {pub x: i32, pub y: i32}

impl From<Point> for [i32; 2] {
    #[inline] fn from(value: Point) -> Self {
        [value.x, value.y]
    }
}

impl Add for Point {
    type Output = Self;
    #[inline] fn add(self, rhs: Self) -> Self::Output {
        Self{x: self.x + rhs.x, y: self.y + rhs.y}
    }
}

impl AddAssign for Point {
    #[inline] fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs
    }
}

impl Sub for Point {
    type Output = Self;
    #[inline] fn sub(self, rhs: Self) -> Self::Output {
        Self{x: self.x - rhs.x, y: self.y - rhs.y}
    }
}

impl SubAssign for Point {
    #[inline] fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs
    }
}

impl Neg for Point {
    type Output = Self;
    #[inline] fn neg(self) -> Self::Output {
        Self{x: -self.x, y: -self.y}
    }
}

impl LooseEq for Point {
    #[inline] fn loose_eq(&self, value: Self, off: Self) -> bool {
        self.x.loose_eq(value.x, off.x)
            && self.y.loose_eq(value.y, off.y)
    }
}

impl Point {
    pub const ZERO: Self = Self{x:0, y:0};
    #[inline] pub fn avg(self, p2: Self) -> Self {
        Self{x: ((self.x as i64 + p2.x as i64) / 2) as i32,
            y: ((self.y as i64 + p2.y as i64) / 2) as i32}
    }

    #[inline] pub fn clamp_x(mut self, min: i32, max: i32) -> Self {
        self.x = total_clamp(self.x, min, max);
        self
    }

    #[inline] pub fn clamp_y(mut self, min: i32, max: i32) -> Self {
        self.y = total_clamp(self.y, min, max);
        self
    }

    #[inline] pub fn normalise(mut self, old_space: Rect, new_space: Rect) -> Self {
        self.y = (((self.y - old_space.bottom()) as f32 / old_space.height() as f32)
            * new_space.height() as f32) as i32 + new_space.bottom();
        self.x = (((self.x - old_space.left()) as f32 / old_space.width() as f32)
            * new_space.width() as f32) as i32 + new_space.left();
        self
    }

    #[inline]
    pub fn shift_x(self, off: i32) -> Self {Self{x: self.x + off, y: self.y}}

    #[inline]
    pub fn shift_y(self, off: i32) -> Self {Self{x: self.x, y: self.y + off}}

    #[inline] pub fn map<T>(self, mut f: impl FnMut(i32) -> T) -> [T; 2] {
        [f(self.x), f(self.y)]
    }
}

pub trait HitZone: Sized + Debug {
    fn contains(&self, point: Point) -> bool;
    fn     left(&self) -> i32;
    fn      top(&self) -> i32;
    fn    right(&self) -> i32;
    fn   bottom(&self) -> i32;
    fn   center_point(&self) -> Point;
    fn    shift(self, offset: Point)  -> Self;
    fn     draw(&self, ctx: &CanvasRenderingContext2d);

    #[inline] fn   width(&self) -> i32 {self.right() - self.left()}
    #[inline] fn  height(&self) -> i32 {self.bottom() - self.top()}
    #[inline] fn shift_x(self, x: i32) -> Self {self.shift(Point{x, y:0})}
    #[inline] fn shift_y(self, y: i32) -> Self {self.shift(Point{x:0, y})}
}

/// methods' naming convention:
/// *square* - accepts `side` as both `width` & `height`,
///     else accepts the bottom-right corner
/// *zero* - the top-left corner is zero,
///     else accepts the top-left corner as `src`
/// *center* - accepts the center point, the dimensions are accepted halved
#[derive(Debug, Clone, Copy)]
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
    #[inline] fn center_point(&self) -> Point {self.0.avg(self.1)}

    #[inline] fn shift(self, offset: Point) -> Self {
        Self(self.0 + offset, self.1 + offset)
    }

    #[inline] fn draw(&self, ctx: &CanvasRenderingContext2d) {
        ctx.rect(self.left().into(), self.top().into(), self.width().into(), self.height().into())
    }
}

impl From<Point> for Rect {
    /// same as `Rect::zero`
    #[inline] fn from(value: Point) -> Self {
        Rect::zero(value)
    }
}

impl Rect {
    #[inline] pub fn new(top_left: Point, bottom_right: Point) -> Self {
        Self(top_left, bottom_right)
    }

    #[inline] pub fn zero(bottom_right: Point) -> Self {
        Self(Point::ZERO, bottom_right)
    }

    #[inline] pub fn center(center: Point, half_sides: Point) -> Self {
        Self(center - half_sides, center + half_sides)
    }

    #[inline] pub fn zero_center(center: Point) -> Self {
        Self(Point::ZERO, center + center)
    }

    #[inline] pub fn square(src: Point, side: i32) -> Self {
        Self(src, src + Point{x: side, y: side})
    }

    #[inline] pub fn square_center(center: Point, half_side: i32) -> Self {
        let half_sides = Point{x: half_side, y: half_side};
        Self(center - half_sides, center + half_sides)
    }

    #[inline] pub fn square_zero(side: i32) -> Self {
        Self(Point::ZERO, Point{x: side, y: side})
    }

    #[inline] pub fn square_zero_center(half_side: i32) -> Self {
        let side = half_side * 2;
        Self(Point::ZERO, Point{x: side, y: side})
    }

    #[inline] pub fn to_rhombus(self) -> Rhombus {
        Rhombus::new(self.center_point(), self.width() / 2, self.height() / 2)
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
    #[inline] fn center_point(&self) -> Point {self.center}

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
pub struct NanError;

impl Display for NanError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "attempted to convert a NaN to a real")
    }
}

macro_rules! real_from_ints_impl {
    ($real:ty { $float:ty } : $($int:ty),+) => {
        $(
            impl From<$int> for $real {
                #[inline] fn from(x: $int) -> Self {Self(x as $float)}
            }

            impl From<$real> for $int {
                #[inline] fn from(x: $real) -> Self {
                    if      *x >= <$int>::MAX as $float {<$int>::MAX}
                    else if *x <= <$int>::MIN as $float {<$int>::MIN}
                    else {*x as $int}
                }
            }

            impl PartialEq<$int> for $real {
                #[inline] fn eq(&self, other: &$int) -> bool {
                    PartialEq::eq(&self.0, &(*other as $float))
                }
            }

            impl PartialOrd<$int> for $real {
                #[inline] fn partial_cmp(&self, other: &$int) -> Option<Ordering> {
                    PartialOrd::partial_cmp(&self.0, &(*other as $float))
                }
            }
        )+
    };
}

macro_rules! real_float_operator_impl {
    ($real:ty { $float:ty } , $other_float:ty : $($op:ident :: $method:ident | $assign_op:ident :: $assign_method:ident),+) => {
        $(
            impl $op<$other_float> for $real {
                type Output = Self;
                #[inline(always)] fn $method(self, rhs: $other_float) -> Self {
                    let res = Self(self.0.$method(rhs as $float));
                    assert!(!res.0.is_nan());
                    res
                }
            }

            impl $op<&$other_float> for $real {
                type Output = $real;
                #[inline(always)]
                fn $method(self, rhs: &$other_float) -> $real {$op::$method(self, *rhs)}
            }

            impl<'a> $op<$other_float> for &'a $real {
                type Output = $real;
                #[inline(always)]
                fn $method(self, rhs: $other_float) -> $real {$op::$method(*self, rhs)}
            }

            impl<'a> $op<&$other_float> for &'a $real {
                type Output = $real;
                #[inline(always)]
                fn $method(self, rhs: &$other_float) -> $real {$op::$method(*self, *rhs)}
            }

            impl $assign_op<$other_float> for $real {
                #[inline(always)] fn $assign_method(&mut self, rhs: $other_float) {
                    let res = Self(self.0.$method(rhs as $float));
                    assert!(!res.0.is_nan());
                    *self = res;
                }
            }

            impl $assign_op<&$other_float> for $real {
                #[inline(always)]
                fn $assign_method(&mut self, rhs: &$other_float) {$assign_op::$assign_method(self, *rhs)}
            }
        )+
    }
}

macro_rules! real_int_operator_impl {
    ($real:ty { $float:ty }, $int:ty : $($op:ident :: $method:ident | $assign_op:ident :: $assign_method:ident),+) => {
        $(
            impl $op<$int> for $real {
                type Output = Self;
                #[inline(always)]
                fn $method(self, rhs: $int) -> Self {Self(self.0.$method(rhs as $float))}
            }

            impl $op<&$int> for $real {
                type Output = $real;
                #[inline(always)]
                fn $method(self, rhs: &$int) -> $real {$op::$method(self, *rhs)}
            }

            impl<'a> $op<$int> for &'a $real {
                type Output = $real;
                #[inline(always)]
                fn $method(self, rhs: $int) -> $real {$op::$method(*self, rhs)}
            }

            impl<'a> $op<&$int> for &'a $real {
                type Output = $real;
                #[inline(always)]
                fn $method(self, rhs: &$int) -> $real {$op::$method(*self, *rhs)}
            }

            impl $assign_op<$int> for $real {
                #[inline(always)]
                fn $assign_method(&mut self, rhs: $int) {self.0.$assign_method(rhs as $float)}
            }

            impl $assign_op<&$int> for $real {
                #[inline(always)]
                fn $assign_method(&mut self, rhs: &$int) {$assign_op::$assign_method(self, *rhs)}
            }
        )+
    };
}

macro_rules! real_real_operator_impl {
    ($real:ty { $float:ty }, $other_real:ty { $other_float:ty } : $($op:ident :: $method:ident | $assign_op:ident :: $assign_method:ident),+) => {
        $(
            impl $op<$other_real> for $real {
                type Output = Self;
                #[inline] fn $method(self, rhs: $other_real) -> Self {
                    let res = self.0.$method(rhs.0 as $float);
                    assert!(!res.is_nan());
                    Self(res)
                }
            }

            impl $op<&$other_real> for $real {
                type Output = $real;
                #[inline]
                fn $method(self, rhs: &$other_real) -> $real {$op::$method(self, *rhs)}
            }

            impl<'a> $op<$other_real> for &'a $real {
                type Output = $real;
                #[inline]
                fn $method(self, rhs: $other_real) -> $real {$op::$method(*self, rhs)}
            }

            impl<'a> $op<&$other_real> for &'a $real {
                type Output = $real;
                #[inline]
                fn $method(self, rhs: &$other_real) -> $real {$op::$method(*self, *rhs)}
            }

            impl $assign_op<$other_real> for $real {
                #[inline] fn $assign_method(&mut self, rhs: $other_real) {
                    let res = self.0.$method(rhs.0 as $float);
                    assert!(!res.is_nan());
                    self.0 = res;
                }
            }

            impl $assign_op<&$other_real> for $real {
                #[inline(always)]
                fn $assign_method(&mut self, rhs: &$other_real) {$assign_op::$assign_method(self, *rhs)}
            }
        )+
    }
}

macro_rules! real_impl {
    ($real:ident { $float:ident }, $other_real:ty { $other_float:ty }) => {
        #[derive(Debug, Default, PartialEq, Clone, Copy)]
        pub struct $real($float);

        impl Deref for $real {
            type Target = $float;
            #[inline] fn deref(&self) -> &Self::Target {&self.0}
        }

        impl PartialOrd for $real {
            #[inline] fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                self.0.partial_cmp(&other.0)
            }
        }

        impl Ord for $real {
            #[inline] fn cmp(&self, other: &Self) -> Ordering {
                unsafe{self.0.partial_cmp(&other.0).unwrap_unchecked()}
            }
        }

        impl Eq for $real {}

        impl TryFrom<$float> for $real {
            type Error = NanError;
            #[inline] fn try_from(x: $float) -> Result<Self, Self::Error> {
                if x.is_nan() {return Err(NanError)}
                Ok(Self(x))
            }
        }

        impl From<$other_real> for $real {
            #[inline]
            fn from(value: $other_real) -> Self {Self(value.0 as $float)}
        }

        impl Display for $real {
            #[inline] fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                Display::fmt(&self.0, f)
            }
        }

        impl Neg for $real {
            type Output = Self;
            #[inline]
            fn neg(self) -> Self::Output {Self(-self.0)}
        }

        real_from_ints_impl!($real{$float}:
            u8, i8, u16, i16, u32, i32, usize, isize, u64, i64);
        real_int_operator_impl!($real{$float}, u8:
            Add::add|AddAssign::add_assign, Sub::sub|SubAssign::sub_assign,
            Mul::mul|MulAssign::mul_assign, Div::div|DivAssign::div_assign,
            Rem::rem|RemAssign::rem_assign);
        real_int_operator_impl!($real{$float}, i8:
            Add::add|AddAssign::add_assign, Sub::sub|SubAssign::sub_assign,
            Mul::mul|MulAssign::mul_assign, Div::div|DivAssign::div_assign,
            Rem::rem|RemAssign::rem_assign);
        real_int_operator_impl!($real{$float}, u16:
            Add::add|AddAssign::add_assign, Sub::sub|SubAssign::sub_assign,
            Mul::mul|MulAssign::mul_assign, Div::div|DivAssign::div_assign,
            Rem::rem|RemAssign::rem_assign);
        real_int_operator_impl!($real{$float}, i16:
            Add::add|AddAssign::add_assign, Sub::sub|SubAssign::sub_assign,
            Mul::mul|MulAssign::mul_assign, Div::div|DivAssign::div_assign,
            Rem::rem|RemAssign::rem_assign);
        real_int_operator_impl!($real{$float}, u32:
            Add::add|AddAssign::add_assign, Sub::sub|SubAssign::sub_assign,
            Mul::mul|MulAssign::mul_assign, Div::div|DivAssign::div_assign,
            Rem::rem|RemAssign::rem_assign);
        real_int_operator_impl!($real{$float}, i32:
            Add::add|AddAssign::add_assign, Sub::sub|SubAssign::sub_assign,
            Mul::mul|MulAssign::mul_assign, Div::div|DivAssign::div_assign,
            Rem::rem|RemAssign::rem_assign);
        real_int_operator_impl!($real{$float}, usize:
            Add::add|AddAssign::add_assign, Sub::sub|SubAssign::sub_assign,
            Mul::mul|MulAssign::mul_assign, Div::div|DivAssign::div_assign,
            Rem::rem|RemAssign::rem_assign);
        real_int_operator_impl!($real{$float}, isize:
            Add::add|AddAssign::add_assign, Sub::sub|SubAssign::sub_assign,
            Mul::mul|MulAssign::mul_assign, Div::div|DivAssign::div_assign,
            Rem::rem|RemAssign::rem_assign);
        real_int_operator_impl!($real{$float}, u64:
            Add::add|AddAssign::add_assign, Sub::sub|SubAssign::sub_assign,
            Mul::mul|MulAssign::mul_assign, Div::div|DivAssign::div_assign,
            Rem::rem|RemAssign::rem_assign);
        real_int_operator_impl!($real{$float}, i64:
            Add::add|AddAssign::add_assign, Sub::sub|SubAssign::sub_assign,
            Mul::mul|MulAssign::mul_assign, Div::div|DivAssign::div_assign,
            Rem::rem|RemAssign::rem_assign);

        real_float_operator_impl!($real{$float}, $float:
            Add::add|AddAssign::add_assign, Sub::sub|SubAssign::sub_assign,
            Mul::mul|MulAssign::mul_assign, Div::div|DivAssign::div_assign,
            Rem::rem|RemAssign::rem_assign);
        real_float_operator_impl!($real{$float}, $other_float:
            Add::add|AddAssign::add_assign, Sub::sub|SubAssign::sub_assign,
            Mul::mul|MulAssign::mul_assign, Div::div|DivAssign::div_assign,
            Rem::rem|RemAssign::rem_assign);
        real_real_operator_impl!($real{$float}, $real{$float}:
            Add::add|AddAssign::add_assign, Sub::sub|SubAssign::sub_assign,
            Mul::mul|MulAssign::mul_assign, Div::div|DivAssign::div_assign,
            Rem::rem|RemAssign::rem_assign);
        real_real_operator_impl!($real{$float}, $other_real{$other_float}:
            Add::add|AddAssign::add_assign, Sub::sub|SubAssign::sub_assign,
            Mul::mul|MulAssign::mul_assign, Div::div|DivAssign::div_assign,
            Rem::rem|RemAssign::rem_assign);

        impl $real {
            pub const INFINITY: $real = $real($float::INFINITY);
            pub const NEG_INFINITY: $real = $real($float::NEG_INFINITY);
            pub const ZERO: $real = $real(0.0);
            pub const ONE: $real = $real(1.0);
            pub const PI: $real = $real(std::$float::consts::PI);
            pub const TAU: $real = $real(std::$float::consts::TAU);

            #[inline]
            pub const unsafe fn new_unchecked(x: $float) -> Self {Self(x)}

            #[inline]
            pub const fn new_or(default: Self, x: $float) -> Self {
                if x.is_nan() {default} else {Self(x)}
            }

            #[inline]
            pub fn rem_euclid(self, rhs: Self) -> Option<Self> {
                let res = self.0.rem_euclid(rhs.0);
                if res.is_nan() {return None}
                Some(Self(res))
            }

            #[inline]
            pub fn copysign(self, sign: Self) -> Self {
                Self(self.0.copysign(*sign))
            }

            #[inline]
            pub fn recip(self) -> Self {Self(self.0.recip())}

            #[inline]
            pub fn exp2(self) -> Self {Self(self.0.exp2())}

            #[inline]
            pub fn floor(self) -> Self {Self(self.0.floor())}

            #[inline]
            pub fn ceil(self) -> Self {Self(self.0.ceil())}

            #[inline]
            pub fn round(self) -> Self {Self(self.0.round())}

            #[inline] pub fn floor_to(self, step: Self) -> Self {
                match step.classify() {
                    FpCategory::Zero => self,
                    FpCategory::Infinite => step,
                    _ => (self / step).floor() * step
                }
            }

            #[inline]
            pub fn is_finite(&self) -> bool {self.0.is_finite()}

            #[inline]
            pub fn abs(self) -> Self {Self(self.0.abs())}
        }
    };
}

real_impl!(R32{f32}, R64{f64});
real_impl!(R64{f64}, R32{f32});

#[macro_export]
macro_rules! r32 {
    ($x:literal) => {
        unsafe{$crate::utils::R32::new_unchecked($x)}
    };
}

#[macro_export]
macro_rules! r64 {
    ($x:literal) => {
        unsafe{$crate::utils::R64::new_unchecked($x)}
    };
}

#[test]
fn real_round_to() {
    assert!(r64![1.3].floor_to(r64![0.2]).loose_eq(r64![1.2], 0.005));
    assert!(r64![-1.3].floor_to(r64![0.2]).loose_eq(r64![-1.4], 0.005));
}

pub trait SaturatingFrom<T> {
    fn saturating_from(x: T) -> Self;
}

pub trait SaturatingInto<T: SaturatingFrom<Self>>: Sized {
    #[inline(always)]
    fn saturating_into(self) -> T {T::saturating_from(self)}
}

impl<S, D: SaturatingFrom<S>> SaturatingInto<D> for S {}

macro_rules! sat_from_u_impl {
    ($src:ty => $($dst:ty)|+) => {
        $(
            impl SaturatingFrom<$src> for $dst {
                #[inline] fn saturating_from(x: $src) -> $dst {
                    x.min(<$dst>::MAX as $src) as $dst
                }
            }
        )+
    };
}

sat_from_u_impl!(u64   => i64|usize|isize|u32|i32|u16|i16|u8|i8);
sat_from_u_impl!(usize => i64|isize|u32|i32|u16|i16|u8|i8);
sat_from_u_impl!(u32   => isize|i32|u16|i16|u8|i8);
sat_from_u_impl!(u16   => i16|u8|i8);
sat_from_u_impl!(u8    => i8);

macro_rules! sat_from_s_impl {
    ($src:ty => $($dst:ty)|+) => {
        $(
            impl SaturatingFrom<$src> for $dst {
                #[inline] fn saturating_from(x: $src) -> $dst {
                    const DST_SMALLER: bool = (<$dst>::MAX as u64) < <$src>::MAX as u64;
                    if DST_SMALLER {x.clamp(<$dst>::MIN as $src, <$dst>::MAX as $src) as $dst}
                    else {x.max(<$dst>::MIN as $src) as $dst}
                }
            }
        )+
    };
}

sat_from_s_impl!(i64   => u64|usize|isize|u32|i32|u16|i16|u8|i8);
sat_from_s_impl!(isize => u64|usize|u32|i32|u16|i16|u8|i8);
sat_from_s_impl!(i32   => usize|i32|u16|i16|u8|i8);
sat_from_s_impl!(i16   => u16|u8|i8);
sat_from_s_impl!(i8    => u8);

macro_rules! sat_from_real_impl {
    ($src:ty => $($dst:ty)|+) => {
        $(
            impl SaturatingFrom<$src> for $dst {
                #[inline] fn saturating_from(x: $src) -> $dst {
                    *x.clamp(<$dst>::MIN.into(), <$dst>::MAX.into()) as $dst
                }
            }
        )+
    };
}

sat_from_real_impl!(R32 => u64|i64|usize|isize|u32|i32|u16|i16|u8|i8);
sat_from_real_impl!(R64 => u64|i64|usize|isize|u32|i32|u16|i16|u8|i8);
