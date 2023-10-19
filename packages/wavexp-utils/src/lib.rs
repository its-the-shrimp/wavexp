#![feature(let_chains)]
#![feature(const_float_classify)]
#![feature(never_type)]
#![feature(try_blocks)]
#![feature(raw_ref_op)]
#![feature(result_option_inspect)]

pub mod cell;
pub mod iter;
use std::{
    ptr,
    mem::{take, transmute_copy, MaybeUninit, forget},
    fmt::{self, Debug, Formatter, Display},
    ops::{Neg, Range, Deref, RangeBounds, 
        Add, Sub, Mul, Div, Rem,
        AddAssign, SubAssign, MulAssign, DivAssign, RemAssign, RangeInclusive},
    iter::{successors, Sum},
    cmp::Ordering,
    array::from_fn,
    num::{TryFromIntError,
        NonZeroU8, NonZeroU16, NonZeroU32, NonZeroUsize, NonZeroU64,
        NonZeroI8, NonZeroI16, NonZeroI32, NonZeroIsize, NonZeroI64}};
pub use js_sys;
pub use wasm_bindgen;
use wasm_bindgen::{JsValue, JsCast};
use web_sys::{
    Document,
    Window,
    CanvasRenderingContext2d,
    HtmlCanvasElement,
    Element,
    console::{warn_1, warn_2},
    HtmlElement};
use yew::{html::IntoPropValue, AttrValue};

pub fn modify<T>(src: &mut T, f: impl FnOnce(T) -> T) {
    let src = src as *mut T;
    unsafe{src.write(f(src.read()))}
}

pub fn default<T: Default>() -> T {T::default()}

#[repr(transparent)]
pub struct Alias<'a, T: ?Sized>(pub &'a T);

impl<'a, T: ?Sized + Deref> Deref for Alias<'a, T> {
    type Target = T::Target;
    fn deref(&self) -> &Self::Target {self.0.deref()}
}

pub trait Check: Sized {
	fn check(self, f: impl FnOnce(&Self) -> bool) -> Result<Self, Self> {
		if f(&self) {Ok(self)} else {Err(self)}
	}

    fn check_in<R>(self, range: R) -> Result<Self, Self>
	where Self: PartialOrd, R: RangeBounds<Self> {
		if range.contains(&self) {Ok(self)} else {Err(self)}
	}

	fn check_not_in<R>(self, range: R) -> Result<Self, Self>
	where Self: PartialOrd, R: RangeBounds<Self> {
		if !range.contains(&self) {Ok(self)} else {Err(self)}
	}
}
impl<T> Check for T {}

pub trait Tee: Sized {
	fn tee(self, f: impl FnOnce(&Self)) -> Self {
        f(&self); self
	}

    fn js_log(self, label: &str) -> Self 
    where Self: Debug {
        js_log!("{}{:?}", label, &self); self
    }
}
impl<T> Tee for T {}

/// like `ToString`, but for `AttrValue`
pub trait ToAttrValue {
    fn to_attr_value(&self) -> AttrValue;
}

impl<T: ToString> ToAttrValue for T {
    fn to_attr_value(&self) -> AttrValue {
        AttrValue::from(self.to_string())
    }
}

pub trait Pipe: Sized {
	fn pipe<T>(self, f: impl FnOnce(Self) -> T) -> T {
        f(self)
    }

    fn pipe_if(self, cond: bool, f: impl FnOnce(Self) -> Self) -> Self {
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
    fn flip(&mut self);
    fn to_app_result(self) -> AppResult<()>;
}

impl BoolExt for bool {
    fn choose<T>(self, on_true: T, on_false: T) -> T {
        if self {on_true} else {on_false}
    }

    fn then_or<T>(self, default: T, f: impl FnOnce() -> T) -> T {
        if self {f()} else {default}
    }

    fn then_or_else<T>(self, default: impl FnOnce() -> T, f: impl FnOnce() -> T) -> T {
        if self {f()} else {default()}
    }

    fn then_negate<T: Neg<Output=T>>(self, val: T) -> T {
        if self {-val} else {val}
    }

    fn then_try<T, E>(self, f: impl FnOnce() -> Result<T, E>) -> Result<Option<T>, E> {
        self.then(f).transpose()
    }

    fn and_then<T>(self, f: impl FnOnce() -> Option<T>) -> Option<T> {
        if self {f()} else {None}
    }

    fn flip(&mut self) {
        *self = !*self
    }

    fn to_app_result(self) -> AppResult<()> {
        if self {Ok(())}
        else {Err(app_error!("expected `true`, found `false`"))}
    }
}

pub trait ArrayExt<T, const N: usize>: Sized {
    fn zip<O, R>(self, other: [O; N], f: impl FnMut(T, O) -> R) -> [R; N];
    fn zip_fold<O, R>(self, init: R, other: [O; N], f: impl FnMut(R, T, O) -> R) -> R;
    fn add<'a, O>(self, other: &'a [O; N]) -> Self
    where T: AddAssign<&'a O>;
    fn sub<'a, O>(self, other: &'a [O; N]) -> Self
    where T: SubAssign<&'a O>;
    fn mul<'a, O>(self, other: &'a [O; N]) -> Self
    where T: MulAssign<&'a O>;
    fn div<'a, O>(self, other: &'a [O; N]) -> Self
    where T: DivAssign<&'a O>;
    fn rem<'a, O>(self, other: &'a [O; N]) -> Self
    where T: RemAssign<&'a O>;
    fn floor_to(self, other: Self) -> Self
    where T: RoundTo;
    fn ceil_to(self, other: Self) -> Self
    where T: RoundTo;
    fn sum<R>(self) -> R
    where R: Sum<T>;
    fn array_check_in<R, O>(self, ranges: &[R; N]) -> Option<Self>
    where T: PartialOrd<O>, O: PartialOrd<T>, R: RangeBounds<O>;
    fn fit<R, O>(&self, values: [R; N]) -> [R; N]
    where T: RangeExt<O>, O: Clone + PartialOrd<R>, R: Clone + From<O>;
}

impl<T, const N: usize> ArrayExt<T, N> for [T; N] {
    fn zip<O, R>(self, other: [O; N], mut f: impl FnMut(T, O) -> R) -> [R; N] {
        let (mut d, mut s) = (self.into_iter(), other.into_iter());
        from_fn(|_| unsafe{f(d.next().unwrap_unchecked(), s.next().unwrap_unchecked())})
    }

    fn zip_fold<O, R>(self, init: R, other: [O; N], mut f: impl FnMut(R, T, O) -> R) -> R {
        self.into_iter().zip(other).fold(init, |r, (x, y)| f(r, x, y))
    }

    fn add<'a, O>(mut self, other: &'a [O; N]) -> Self where T: AddAssign<&'a O> {
        for (dst, src) in self.iter_mut().zip(other.iter()) {*dst += src}
        self
    }

    fn sub<'a, O>(mut self, other: &'a [O; N]) -> Self where T: SubAssign<&'a O> {
        for (dst, src) in self.iter_mut().zip(other.iter()) {*dst -= src}
        self
    }

    fn mul<'a, O>(mut self, other: &'a [O; N]) -> Self where T: MulAssign<&'a O> {
        for (dst, src) in self.iter_mut().zip(other.iter()) {*dst *= src}
        self
    }

    fn div<'a, O>(mut self, other: &'a [O; N]) -> Self where T: DivAssign<&'a O> {
        for (d, s) in self.iter_mut().zip(other.iter()) {*d /= s}
        self
    }

    fn rem<'a, O>(mut self, other: &'a [O; N]) -> Self where T: RemAssign<&'a O> {
        for (d, s) in self.iter_mut().zip(other.iter()) {*d %= s}
        self
    }

    fn floor_to(mut self, other: Self) -> Self where T: RoundTo {
        for (d, s) in self.iter_mut().zip(other) {modify(d, |d| d.floor_to(s))}
        self
    }

    fn ceil_to(mut self, other: Self) -> Self where T: RoundTo {
        for (d, s) in self.iter_mut().zip(other) {modify(d, |d| d.ceil_to(s))}
        self
    }

    fn sum<R>(self) -> R where R: Sum<T> {self.into_iter().sum()}

    fn array_check_in<R, O>(self, ranges: &[R; N]) -> Option<Self>
    where T: PartialOrd<O>, O: PartialOrd<T>, R: RangeBounds<O> {
        self.iter().zip(ranges).all(|(i, r)| r.contains(i)).then_some(self)
    }

    fn fit<R, O>(&self, mut values: [R; N]) -> [R; N]
    where T: RangeExt<O>, O: Clone + PartialOrd<R>, R: Clone + From<O> {
        for (i, r) in values.iter_mut().zip(self) {
            *i = r.fit(i.clone());
        }
        values
    }
}

pub trait ArrayFrom<T, const N: usize>: Sized {
    fn array_from(x: T) -> [Self; N];
}

impl<S, D, const N: usize> ArrayFrom<[S; N], N> for D where D: From<S> {
    fn array_from(x: [S; N]) -> [Self; N] {x.map(D::from)}
}

pub trait IntoArray<T, const N: usize> {
    fn into_array(self) -> [T; N];
}

impl<T, U, const N: usize> IntoArray<U, N> for T where U: ArrayFrom<T, N> {
    fn into_array(self) -> [U; N] {U::array_from(self)}
}

pub trait FlippedArray<T, const OUTER: usize, const INNER: usize> {
    fn flipped(self) -> [[T; OUTER]; INNER];
}

impl<T, const OUTER: usize, const INNER: usize> FlippedArray<T, OUTER, INNER> for [[T; INNER]; OUTER] {
    fn flipped(mut self) -> [[T; OUTER]; INNER] {
        unsafe {
            if OUTER == INNER {
                let mut src = self.as_mut_ptr() as *mut T;
                for outer in 0 .. OUTER {
                    src = src.add(outer + 1);
                    for inner in outer + 1 .. INNER {
                        src.swap(src.add((inner - outer) * (INNER - 1)));
                        src = src.add(1);
                    }
                }
                transmute_copy(&self)
            } else {
                let mut new_self: MaybeUninit<_> = MaybeUninit::uninit();
                let mut res = new_self.as_mut_ptr() as *mut T;
                for inner in 0 .. INNER {
                    let mut src = (self.as_mut_ptr() as *mut T).add(inner);
                    for _ in 0 .. OUTER {
                        res.copy_from(src, 1);
                        res = res.add(1);
                        src = src.add(INNER);
                    }
                }
                forget(self);
                new_self.assume_init()
            }
        }
    }
}

pub struct SliceRef<'a, T: ?Sized> {
    inner: &'a T,
    index: usize
}

impl<'a, T> Deref for SliceRef<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {self.inner}
}

impl<'a, T> SliceRef<'a, T> {
    pub fn new(slice: &'a [T], index: usize) -> Option<Self> {
        slice.get(index).map(|inner| Self{inner, index}) 
    }

    /// # Safety
    /// `inner` must be a reference to the `index`-th item in its array
    pub unsafe fn raw(inner: &'a T, index: usize) -> Self {Self{inner, index}}

    pub fn index(&self) -> usize {self.index}
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
        $( res.push(&*$crate::js_types::$t::from($v)); )*
        $crate::wasm_bindgen::JsValue::from(res)
    }};
}

#[macro_export]
macro_rules! js_obj {
	($($t:ident $k:ident : $v:expr),*) => {
		$crate::wasm_bindgen::JsValue::from($crate::js_sys::Map::new()
			$( .set(&$crate::js_types::str::from(stringify!($k)).into(),
				&*$crate::js_types::$t::from($v)) )*)
	}
}

#[macro_export]
macro_rules! js_function {
    (|| $body:expr) => {
        $crate::wasm_bindgen::closure::Closure::<dyn FnMut()>::new(move || $body)
            .into_js_value()
            .unchecked_into::<$crate::js_sys::Function>()
    };
    (|$arg:ident $(: $t:ty)?| $body:expr) => {
        $crate::wasm_bindgen::closure::Closure::<dyn FnMut(_)>::new(move |$arg $(: $t)?| $body)
            .into_js_value()
            .unchecked_into::<$crate::js_sys::Function>()
    };
    ($var:path) => {
        $crate::wasm_bindgen::closure::Closure::new($var)
            .into_js_value()
            .unchecked_into::<$crate::js_sys::Function>()
    };
}

pub use web_sys::console::log_1;
#[macro_export]
macro_rules! js_log {
	($arg:literal) => {
        $crate::log_1(&format!($arg).into())
	};
	($f:literal, $($arg:expr),*) => {
		$crate::log_1(&format!($f, $($arg),*).into())
	}
}

#[macro_export]
macro_rules! js_assert {
    ($($s:tt)+) => {
        if !$($s)+ {
            Err($crate::wasm_bindgen::JsValue::from($crate::js_sys::Error::new(stringify!($($s)+))))
        } else {
            Ok(())
        }
    };
}

#[macro_export]
macro_rules! eval_once {
    ($t:ty : $e:expr) => {{
        static RES: $crate::cell::WasmCell<std::cell::OnceCell<$t>>
            = $crate::cell::WasmCell(std::cell::OnceCell::new());
        RES.get_or_init(|| $e)
    }};
}

pub fn window() -> Window {
	unsafe{web_sys::window().unwrap_unchecked()}
}

pub fn document() -> Document {
	unsafe{web_sys::window().unwrap_unchecked().document().unwrap_unchecked()}
}

/// returns precise current time in seconds.
pub fn now() -> Option<R64> {
    window().performance().map(|p| unsafe{R64::new_unchecked(p.now())} / 1000)
}

pub fn report_err(err: js_sys::Error) {
    warn_2(&err,
        &js_sys::Reflect::get(err.as_ref(), &"stack".into()).unwrap_or_else(|e| e));
    if let Some(x) = document().element_dyn_into::<HtmlElement>("error-sign") {
        x.set_hidden(false)
    } else {
        warn_1(&JsValue::from("#error-sign element not found in the DOM"))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AppError(js_sys::Error);

impl From<JsValue> for AppError {
    fn from(value: JsValue) -> Self {
        match value.dyn_into() {
            Ok(x) => Self(x),
            Err(x) => Self::new(&String::from(js_sys::Object::from(x).to_string()))
        }
    }
}

/// `format!`-like macro to create an `AppError`
#[macro_export]
macro_rules! app_error {
    ($x:literal $(,)? $($arg:tt)*) => {
        $crate::AppError::new(&format!($x, $($arg)*))
    };
}

macro_rules! impl_into_app_error {
    ($($t:ty)+) => {
        $(
            impl From<$t> for AppError {
                fn from(value: $t) -> Self {
                    Self::new(&value.to_string())
                }
            }
        )+
    };
}

impl_into_app_error!(hound::Error);

impl From<AppError> for js_sys::Error {
    fn from(value: AppError) -> Self {value.0}
}

impl AppError {
    pub fn new(msg: &str) -> Self {
        Self(js_sys::Error::new(msg))
    }
}

pub type AppResult<T> = Result<T, AppError>;

pub trait AppResultUtils<T>: Sized {
	fn report(self) -> Option<T>;
}

impl<T> AppResultUtils<T> for AppResult<T> {
    fn report(self) -> Option<T> {
        match self {
            Ok(x) => Some(x),
            Err(e) => {report_err(e.into()); None}
        }
    }
}

pub trait ResultExt<T, E> {
    fn to_app_result(self) -> AppResult<T> where E: Display;
    fn explain_err(self, msg: &str) -> AppResult<T>;
    fn explain_err_with(self, f: impl FnOnce() -> String) -> AppResult<T>;
    fn map_or_default<U: Default>(self, f: impl FnOnce(T) -> U) -> U;
}

impl<T, E> ResultExt<T, E> for Result<T, E> {
    fn to_app_result(self) -> AppResult<T> where E: Display {
        self.map_err(|e| AppError::new(&e.to_string()))
    }

    fn explain_err(self, msg: &str) -> AppResult<T> {
        self.map_err(|_| AppError::new(msg))
    }

    fn explain_err_with(self, f: impl FnOnce() -> String) -> AppResult<T> {
        self.map_err(|_| AppError::new(&f()))
    }

    fn map_or_default<U: Default>(self, f: impl FnOnce(T) -> U) -> U {
        match self {Ok(x) => f(x), Err(_) => U::default()}
    }
}

pub trait OptionExt<T> {
    fn to_app_result(self) -> AppResult<T>;
    fn map_or_default<U: Default>(self, f: impl FnOnce(T) -> U) -> U;
    fn choose<U>(&self, on_some: U, on_none: U) -> U;
    fn drop(self) -> Option<()>;
    fn get_or_try_insert<E>(&mut self, f: impl FnOnce() -> Result<T, E>) -> Result<&mut T, E>;
}

impl<T> OptionExt<T> for Option<T> {
    fn to_app_result(self) -> AppResult<T> {
        self.ok_or_else(|| app_error!("`Option` contained the `None` value"))
    }

    fn map_or_default<U: Default>(self, f: impl FnOnce(T) -> U) -> U {
        match self {Some(x) => f(x), None => U::default()}
    }

    fn choose<U>(&self, on_some: U, on_none: U) -> U {
        if self.is_some() {on_some} else {on_none}
    }

    #[allow(clippy::manual_map)]
    fn drop(self) -> Option<()> {
        match self {Some(_) => Some(()), None => None}
    }

    fn get_or_try_insert<E>(&mut self, f: impl FnOnce() -> Result<T, E>) -> Result<&mut T, E> {
        if self.is_none() {
            *self = Some(f()?);
        }
        Ok(unsafe{self.as_mut().unwrap_unchecked()})
    }
}

pub trait HtmlCanvasExt {
    fn get_2d_context(&self) -> AppResult<CanvasRenderingContext2d>;
    fn rect(&self) -> Rect;
    fn size(&self) -> [u32; 2];
    fn sync(&self);
}

impl HtmlCanvasExt for HtmlCanvasElement {
    fn get_2d_context(&self) -> AppResult<CanvasRenderingContext2d> {
        Ok(self.get_context("2d")?.to_app_result()?.unchecked_into())
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
    fn element_dyn_into<T: JsCast>(&self, id: &str) -> Option<T>;
}

impl HtmlDocumentExt for Document {
    fn element_dyn_into<T: JsCast>(&self, id: &str) -> Option<T> {
        self.get_element_by_id(id)?.dyn_into::<T>().ok()
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SliceMove {pub from: usize, pub to: usize}

impl SliceMove {
    pub fn apply(&self, ids: &mut [usize]) {
        let (coef, range) = match self.to.cmp(&self.from) {
            Ordering::Less => (1, self.to .. self.from),
            Ordering::Equal => return,
            Ordering::Greater => (-1, self.from .. self.to)
        };

        for id in ids {
            if *id == self.from {
                *id = self.to;
            } else if range.contains(id) {
                // not going to wrap anyway
                *id = id.wrapping_add_signed(coef);
            }
        }
    }
}

pub struct IterMutWithCtx<'a, T: 'a + Copy> {
    slice: &'a mut [T],
    state: usize
}

impl<'a, T: 'a + Copy> Iterator for IterMutWithCtx<'a, T> {
    type Item = (&'a mut [T], T);
    fn next(&mut self) -> Option<Self::Item> {
        self.slice.get(self.state).copied().map(|x| unsafe {
            self.state += 1;
            ((self.slice as *mut [T]).as_mut().unwrap_unchecked(), x)})
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.slice.len().pipe(|x| (x, Some(x)))
    }
}

impl<'a, T: 'a + Copy> ExactSizeIterator for IterMutWithCtx<'a, T> {
    fn len(&self) -> usize {self.slice.len()}
}

impl<'a, T: 'a + Copy> IterMutWithCtx<'a, T> {
    fn new(slice: &'a mut [T]) -> Self {Self{slice, state: 0}}
}

pub trait SliceExt<T> {
    fn any(&self, f: impl FnMut(&T) -> bool) -> bool;
    fn all(&self, f: impl FnMut(&T) -> bool) -> bool;
    fn to_box(&self) -> Box<Self> where T: Clone;
    fn get_saturating(&self, id: usize) -> &T;
    fn get_saturating_mut(&mut self, id: usize) -> &mut T;
    fn get_wrapping(&self, id: usize) -> &T;
    fn get_wrapping_mut(&mut self, id: usize) -> &mut T;
    fn get_var<'a>(&'a self, ids: &[usize]) -> Option<Vec<&'a T>>;
    fn get_var_mut<'a>(&'a mut self, ids: &[usize]) -> Option<Vec<&'a mut T>>;
    /// # Safety
    /// `index` must be a valid index into `self`
    unsafe fn reorder_unchecked(&mut self, index: usize) -> SliceMove
        where T: Ord;
    // unsafe fn reorder_unchecked_by<F>(&mut self, index: usize, f: F) -> usize
    //  where F: FnMut(&T, &T) -> Ordering
    /// # Safety
    /// `index` must be a valid index into `self`
    unsafe fn reorder_unchecked_by_key<K, F>(&mut self, index: usize, f: F) -> SliceMove
        where F: FnMut(&T) -> K, K: Ord;
    fn reorder(&mut self, index: usize) -> AppResult<SliceMove>
        where T: Ord;
    // fn reorder_by<F>(&mut self, index: usize, f: F) -> Result<usize, ReorderError>
    //  where F: FnMut(&T, &T) -> Ordering
    // fn reorder_by_key<K, F>(&mut self, index: usize, f: F) -> Result<usize, ReorderError>
    //  where F: FnMut(&T) -> K, K: Ord
    fn set_sorted(&mut self, index: usize, value: T) -> AppResult<SliceMove>
        where T: Ord;
    // fn set_sorted_by<F>(&mut self, index: usize, value: T, f: F) -> Result<usize, SetSortedError>
    //  where F: FnMut(&T, &T) -> Ordering
    // fn set_sorted_by_key<K, F>(&mut self, index: usize, value: T, f: F) -> Result<usize, SetSortedError>
    //  where F: FnMut(&T) -> K, K: Ord
    fn get_aware(&self, index: usize) -> Option<SliceRef<'_, T>>;
    /// # Safety
    /// `index` must be a valid index into `self`
    unsafe fn get_unchecked_aware(&self, index: usize) -> SliceRef<'_, T>;
    fn iter_mut_with_ctx<'a>(&'a mut self) -> IterMutWithCtx<'a, T> where T: 'a + Copy;
}

impl<T> SliceExt<T> for [T] {
    fn any(&self, mut f: impl FnMut(&T) -> bool) -> bool {
        let mut res = false;
        for i in self {res |= f(i)}
        res
    }

    fn all(&self, mut f: impl FnMut(&T) -> bool) -> bool {
        let mut res = true;
        for i in self {res &= f(i)}
        res
    }

    fn to_box(&self) -> Box<Self> where T: Clone {self.into()}

    fn get_saturating(&self, id: usize) -> &T {
        unsafe{self.get_unchecked(id.min(self.len() - 1))}
    }

    fn get_saturating_mut(&mut self, id: usize) -> &mut T {
        unsafe{self.get_unchecked_mut(id.min(self.len() - 1))}
    }

    fn get_wrapping(&self, id: usize) -> &T {
        unsafe{self.get_unchecked(id % self.len())}
    }

    fn get_wrapping_mut(&mut self, id: usize) -> &mut T {
        unsafe{self.get_unchecked_mut(id % self.len())}
    }


    fn get_var<'a>(&'a self, ids: &[usize]) -> Option<Vec<&'a T>> {
        let len = self.len();
        for (id, rest) in successors(ids.split_first(), |x| x.1.split_first()) {
            if *id >= len || rest.contains(id) {return None}
        }
        // at this point, `ids` is guaranteed to contain unique valid indices into `self`
        let base = self.as_ptr();
        Some(ids.iter().map(|x| unsafe{&*base.add(*x)}).collect())
    }

    fn get_var_mut<'a>(&'a mut self, ids: &[usize]) -> Option<Vec<&'a mut T>> {
        let len = self.len();
        for (id, rest) in successors(ids.split_first(), |x| x.1.split_first()) {
            if *id >= len || rest.contains(id) {return None}
        }
        // at this point, `ids` is guaranteed to contain unique valid indices into `self`
        let base = self.as_mut_ptr();
        Some(ids.iter().map(|x| unsafe{&mut *base.add(*x)}).collect())
    }

    unsafe fn reorder_unchecked(&mut self, index: usize) -> SliceMove where T: Ord {
        let element = self.get_unchecked(index);
        let (new, should_move) = self.get_unchecked(..index).binary_search(element)
            .map_or_else(|x| (x, x != index), |x| (x, x < index - 1));
        if should_move {
            self.get_unchecked_mut(new..=index).rotate_right(1);
            return SliceMove{from: index, to: new}
        }
        let new = self.get_unchecked(index + 1 ..).binary_search(element)
            .unwrap_or_else(|x| x) + index;
        if new > index {
            self.get_unchecked_mut(index..=new).rotate_left(1);
        }
        SliceMove{from: index, to: new}
    }

    unsafe fn reorder_unchecked_by_key<K, F>(&mut self, index: usize, mut f: F) -> SliceMove
    where F: FnMut(&T) -> K, K: Ord {
        let key = f(self.get_unchecked(index));
        let (new, should_move) = self.get_unchecked(..index).binary_search_by_key(&key, &mut f)
            .map_or_else(|x| (x, x != index), |x| (x, x < index - 1));
        if should_move {
            self.get_unchecked_mut(new..=index).rotate_right(1);
            return SliceMove{from: index, to: new}}
        let new = self.get_unchecked(index+1..).binary_search_by_key(&key, &mut f)
            .unwrap_or_else(|x| x) + index;
        if new > index {
            self.get_unchecked_mut(index..=new).rotate_left(1);
        }
        SliceMove{from: index, to: new}
    }

    fn reorder(&mut self, index: usize) -> AppResult<SliceMove> where T: Ord {
        let len = self.len();
        if index >= len {
            return Err(AppError::new(&format!("reorder index (is {index}) should be < len (is {len})")))
        }
        Ok(unsafe{self.reorder_unchecked(index)})
    }

    fn set_sorted(&mut self, index: usize, value: T) -> AppResult<SliceMove> where T: Ord {
        let len = self.len();
        if index >= len {
            return Err(AppError::new(&format!("reorder index (is {index}) should be < len (is {len})")))
        }
        Ok(unsafe {
            let dst = self.get_unchecked_mut(index);
            let should_reorder = &value != dst;
            *dst = value;
            if should_reorder {self.reorder_unchecked(index)} else {SliceMove{from: index, to: index}}
        })
    }

    fn get_aware(&self, index: usize) -> Option<SliceRef<'_, T>> {SliceRef::new(self, index)}

    unsafe fn get_unchecked_aware(&self, index: usize) -> SliceRef<'_, T> {
        SliceRef::raw(self.get_unchecked(index), index)
    }

    fn iter_mut_with_ctx<'a>(&'a mut self) -> IterMutWithCtx<'a, T> where T: 'a + Copy {
        IterMutWithCtx::new(self)
    }
}

#[test] fn slice_get_var() {
    let x = [1, 2, 4, 8, 16, 32, 64];
    assert_eq!(x.get_var(&[1, 3, 6]), Some(vec![&2, &8, &64]));
    assert_eq!(x.get_var(&[1, 25]), None);
    assert_eq!(x.get_var(&[1, 4, 5, 1]), None);
}

#[test] fn slice_get_var_mut() {
    let mut x = [1, 2, 4, 8, 16, 32, 64];
    assert_eq!(x.get_var_mut(&[1, 3, 6]), Some(vec![&mut 2, &mut 8, &mut 64]));
    assert_eq!(x.get_var_mut(&[1, 25]), None);
    assert_eq!(x.get_var_mut(&[1, 4, 5, 1]), None);
}

#[test] fn slice_reorder() {
    let mut x = [1, 2, 4, 8, 16, 32, 64];
    let old_x = x;
    assert_eq!(x.reorder(3), Ok(SliceMove{from: 3, to: 3}));
    assert_eq!(x, old_x);
    x[1] = 17;
    assert_eq!(x.reorder(1), Ok(SliceMove{from: 1, to: 4}));
    // [1, 2, 4, 8, 16, 32, 64] > [1, 4, 8, 16, 17, 32, 64]
    x[5] = 3;
    assert_eq!(x.reorder(5), Ok(SliceMove{from: 5, to: 1}));
    // [1, 4, 8, 16, 17, 32, 64] > [1, 3, 4, 8, 16, 17, 64]
    let old_x = x;
    assert!(x.reorder(69).is_err());
    assert_eq!(x, old_x);
    x[2] = 3;
    assert_eq!(x.reorder(2), Ok(SliceMove{from: 2, to: 2}));
}

pub trait VecExt<T> {
    fn try_remove(&mut self, index: usize) -> AppResult<T>;
    /// # Safety
    /// `index` must be a valid index into `self`
    unsafe fn remove_unchecked(&mut self, index: usize) -> T;
    fn try_swap_remove(&mut self, index: usize) -> AppResult<T>;
    fn try_insert(&mut self, index: usize, element: T) -> AppResult<&mut T>;
    fn push_unique(&mut self, value: T, f: impl Fn(&T, &T) -> bool) -> bool;
    fn push_sorted(&mut self, value: T) -> usize where T: Ord;
    fn push_sorted_by(&mut self, value: T, f: impl Fn(&T, &T) -> Ordering) -> usize;
    fn push_sorted_by_key<K: Ord>(&mut self, value: T, f: impl FnMut(&T) -> K) -> usize;
}

impl<T> VecExt<T> for Vec<T> {
    fn try_remove(&mut self, index: usize) -> AppResult<T> {
        let len = self.len();
        if index >= len {
            return Err(AppError::new(&format!("removal index (is {index}) should be < len (is {len})")))
        }
        unsafe {
            let ptr = self.as_mut_ptr().add(index);
            let ret = ptr::read(ptr);
            ptr::copy(ptr.add(1), ptr, len - index - 1);
            self.set_len(len - 1);
            Ok(ret)
        }
    }

    unsafe fn remove_unchecked(&mut self, index: usize) -> T {
        let len = self.len();
        let ptr = self.as_mut_ptr().add(index);
        let ret = ptr::read(ptr);
        ptr::copy(ptr.add(1), ptr, len - index - 1);
        self.set_len(len - 1);
        ret
    }

    fn try_swap_remove(&mut self, index: usize) -> AppResult<T> {
        let len = self.len();
        if index >= len {
            return Err(AppError::new(&format!("removal index (is {index}) should be < len (is {len})")))
        }
        unsafe {
            let value = ptr::read(self.as_ptr().add(index));
            let base_ptr = self.as_mut_ptr();
            ptr::copy(base_ptr.add(len - 1), base_ptr.add(index), 1);
            self.set_len(len - 1);
            Ok(value)
        }
    }

    fn try_insert(&mut self, index: usize, element: T) -> AppResult<&mut T> {
        let len = self.len();
        if index > len {
            return Err(AppError::new(&format!("insertion index (is {index}) should be <= len (is {len})")))
        }
        if len == self.capacity() {
            self.try_reserve(1).to_app_result()?;
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
    fn take(&mut self) -> Self {take(self)}
}
impl<T: Default> Take for T {}

pub trait RangeExt<T> {
    type RangeTy<R>;
    fn ordered(self) -> Self where T: Ord;
    fn overlap<O>(&self, other: &Self::RangeTy<O>) -> bool where O: PartialOrd<T>, T: PartialOrd<O>;
    fn loose_contain<O, I>(&self, item: I, offset: O) -> bool where
    O: Copy,
    I: PartialOrd<T> + Add<O, Output=I> + Sub<O, Output=I> + Copy,
    T: PartialOrd<I>;
    fn fit<R>(&self, item: R) -> R where T: Clone + Into<R> + PartialOrd<R>;
    /// if `value` is outside of `self`, extend `self` just enough for `value` to be inside it
    fn extend<R>(self, value: R) -> Self where T: PartialOrd<R> + From<R>;
    /// turns `x .. y` into `f(x) .. f(y)`
    fn map_bounds<R>(self, f: impl FnMut(T) -> R) -> Self::RangeTy<R>;
    fn to_pair(self) -> [T; 2];
}

impl<T> RangeExt<T> for Range<T> {
    type RangeTy<R> = Range<R>;

    fn ordered(self) -> Self where T: Ord {
        if self.start > self.end {self.end .. self.start} else {self}
    }

    fn overlap<O>(&self, other: &Self::RangeTy<O>) -> bool where O: PartialOrd<T>, T: PartialOrd<O> {
        self.contains(&other.start)
            || self.contains(&other.end)
            || other.contains(&self.start)
    }

    fn loose_contain<O, I>(&self, item: I, offset: O) -> bool
    where O: Copy,
    I: PartialOrd<T> + Add<O, Output=I> + Sub<O, Output=I> + Copy,
    T: PartialOrd<I> {
        self.overlap(&(item - offset .. item + offset))
    }

    fn fit<R>(&self, item: R) -> R where T: Clone + Into<R> + PartialOrd<R> {
        if      self.end   < item {self.end.clone().into()}
        else if self.start > item {self.start.clone().into()}
        else                      {item}
    }

    fn extend<R>(self, value: R) -> Self where T: PartialOrd<R> + From<R> {
        if      self.start > value {value.into() .. self.end}
        else if self.end  <= value {self.start .. value.into()}
        else {self}
    }

    fn map_bounds<R>(self, mut f: impl FnMut(T) -> R) -> Self::RangeTy<R> {
        f(self.start) .. f(self.end)
    }

    fn to_pair(self) -> [T; 2] {[self.start, self.end]}
}

impl<T> RangeExt<T> for RangeInclusive<T> {
    type RangeTy<R> = RangeInclusive<R>;

    fn ordered(self) -> Self where T: Ord {
        if self.start() > self.end() {
            let (start, end) = self.into_inner();
            end ..= start
        } else {self}
    }

    fn overlap<O>(&self, other: &Self::RangeTy<O>) -> bool where O: PartialOrd<T>, T: PartialOrd<O> {
        self.contains(other.start())
            || self.contains(other.end())
            || other.contains(self.start())
    }

    fn loose_contain<O, I>(&self, item: I, offset: O) -> bool
    where O: Copy,
    I: PartialOrd<T> + Add<O, Output=I> + Sub<O, Output=I> + Copy,
    T: PartialOrd<I> {
        self.overlap(&(item - offset ..= item + offset))
    }

    fn fit<R>(&self, item: R) -> R where T: Clone + Into<R> + PartialOrd<R> {
        if      self.end()   < &item {self.end().clone().into()}
        else if self.start() > &item {self.start().clone().into()}
        else                         {item}
    }

    fn extend<R>(self, value: R) -> Self where T: PartialOrd<R> + From<R> {
        if      self.start() > &value {value.into() ..= self.into_inner().1}
        else if self.end()   < &value {self.into_inner().0 ..= value.into()}
        else {self}
    }

    fn map_bounds<R>(self, mut f: impl FnMut(T) -> R) -> Self::RangeTy<R> {
        let (start, end) = self.into_inner();
        f(start) ..= f(end)
    }

    fn to_pair(self) -> [T; 2] {self.into_inner().into()}
}

pub trait LooseEq<O = Self> {
    fn loose_eq(&self, value: Self, off: O) -> bool;
    fn loose_ne(&self, value: Self, off: O) -> bool
    where Self: Sized {
        !self.loose_eq(value, off)
    }
}

impl<O: Copy, T: PartialOrd + Add<O, Output=Self> + Sub<O, Output=Self> + Copy> LooseEq<O> for T {
    fn loose_eq(&self, value: Self, off: O) -> bool {
        (value - off .. value + off).contains(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Point {pub x: i32, pub y: i32}

impl From<[R64; 2]> for Point {
    fn from(value: [R64; 2]) -> Self {
        Self{x: value[0].into(), y: value[1].into()}
    }
}

impl<D> ArrayFrom<Point, 2> for D where D: From<i32> {
    fn array_from(x: Point) -> [Self; 2] {[x.x.into(), x.y.into()]}
}

impl Add for Point {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        self.checked_add(rhs).to_app_result().report().unwrap_or(self)
    }
}

impl AddAssign for Point {
    fn add_assign(&mut self, rhs: Self) {
        self.checked_add_assign(rhs).to_app_result().report();
    }
}

impl Sub for Point {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        self.checked_sub(rhs).to_app_result().report().unwrap_or(self)
    }
}

impl SubAssign for Point {
    fn sub_assign(&mut self, rhs: Self) {
        self.checked_sub_assign(rhs).to_app_result().report();
    }
}

impl Neg for Point {
    type Output = Self;
    fn neg(self) -> Self::Output {
        self.checked_neg().to_app_result().report().unwrap_or(self)
    }
}

impl LooseEq for Point {
    fn loose_eq(&self, value: Self, off: Self) -> bool {
        self.x.loose_eq(value.x, off.x)
            && self.y.loose_eq(value.y, off.y)
    }
}

impl RoundTo for Point {
    fn floor_to(self, step: Self) -> Self {
        Self{x: self.x.floor_to(step.x), y: self.y.floor_to(step.y)}
    }

    fn ceil_to(self, step: Self) -> Self {
        Self{x: self.x.ceil_to(step.x), y: self.y.ceil_to(step.y)}
    }
}

impl Point {
    pub const ZERO: Self = Self{x:0, y:0};

    pub fn is_zero(&self) -> bool {self.x == 0 && self.y == 0}
    pub fn nonzero(self) -> Option<Self> {if self.is_zero() {None} else {Some(self)}}

    pub fn normalise(mut self, old_space: Rect, new_space: Rect) -> Self {
        self.y = (((self.y - old_space.bottom()) as f32 / old_space.height() as f32)
            * new_space.height() as f32) as i32 + new_space.bottom();
        self.x = (((self.x - old_space.left()) as f32 / old_space.width() as f32)
            * new_space.width() as f32) as i32 + new_space.left();
        self
    }

    pub fn map<T>(self, mut f: impl FnMut(i32) -> T) -> [T; 2] {
        [f(self.x), f(self.y)]
    }

    pub fn checked_add(self, rhs: Self) -> Option<Self> {
        Some(Self{x: self.x.checked_add(rhs.x)?, y: self.y.checked_add(rhs.y)?})
    }

    pub fn checked_add_assign(&mut self, rhs: Self) -> bool {
        if let Some(x) = self.checked_add(rhs) {*self = x; true}
        else {false}
    }

    pub fn checked_sub(self, rhs: Self) -> Option<Self> {
        Some(Self{x: self.x.checked_sub(rhs.x)?, y: self.y.checked_sub(rhs.y)?})
    }

    pub fn checked_sub_assign(&mut self, rhs: Self) -> bool {
        if let Some(x) = self.checked_sub(rhs) {*self = x; true}
        else {false}
    }

    pub fn checked_neg(self) -> Option<Self> {
        Some(Self{x: self.x.checked_neg()?, y: self.y.checked_neg()?})
    }

    pub fn checked_neg_assign(&mut self) -> bool {
        if let Some(x) = self.checked_neg() {*self = x; true}
        else {false}
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Rect(Point, Point);

impl Rect {
    fn    left(&self) -> i32 {self.0.x}
    fn  bottom(&self) -> i32 {self.1.y}
    fn   width(&self) -> i32 {self.1.x - self.0.x}
    fn  height(&self) -> i32 {self.1.y - self.0.y}
}

pub trait RoundTo {
    fn floor_to(self, step: Self) -> Self;
    fn  ceil_to(self, step: Self) -> Self;
}

macro_rules! round_to_4ints {
    ($($int:ty)+) => {$(
        impl RoundTo for $int {
            fn floor_to(self, step: Self) -> Self {self - self % step}
            fn  ceil_to(self, step: Self) -> Self {step - (self - 1) % step + self - 1}
        }
    )+};
}

round_to_4ints!(i8 u8 i16 u16 i32 u32 isize usize i64 u64);

macro_rules! real_from_unsigned_ints_impl {
    ($real:ty { $float:ty } : $($nonzero:ty{ $int:ty }),+) => {
        $(
            impl From<$int> for $real {
                fn from(x: $int) -> Self {Self(x as $float)}
            }

            impl IntoPropValue<$real> for $int {
                fn into_prop_value(self) -> $real {self.into()}
            }

            impl From<$real> for $int {
                fn from(x: $real) -> Self {
                    if      *x >= <$int>::MAX as $float {<$int>::MAX}
                    else if *x <= <$int>::MIN as $float {<$int>::MIN}
                    else {*x as $int}
                }
            }

            impl IntoPropValue<$int> for $real {
                fn into_prop_value(self) -> $int {self.into()}
            }

            impl PartialEq<$int> for $real {
                fn eq(&self, other: &$int) -> bool {
                    PartialEq::eq(&self.0, &(*other as $float))
                }
            }

            impl PartialOrd<$int> for $real {
                fn partial_cmp(&self, other: &$int) -> Option<Ordering> {
                    PartialOrd::partial_cmp(&self.0, &(*other as $float))
                }
            }

            impl From<$nonzero> for $real {
                fn from(x: $nonzero) -> Self {Self(x.get() as $float)}
            }

            impl IntoPropValue<$real> for $nonzero {
                fn into_prop_value(self) -> $real {self.into()}
            }

            impl From<$real> for $nonzero {
                fn from(x: $real) -> Self {
                    if      *x >= <$int>::MAX as $float {<$nonzero>::MAX}
                    else if *x <= 1.0 {<$nonzero>::MIN}
                    else {unsafe{<$nonzero>::new_unchecked(*x as $int)}}
                }
            }

            impl IntoPropValue<$nonzero> for $real {
                fn into_prop_value(self) -> $nonzero {self.into()}
            }

            impl PartialEq<$nonzero> for $real {
                fn eq(&self, other: &$nonzero) -> bool {
                    PartialEq::eq(&self.0, &(other.get() as $float))
                }
            }

            impl PartialOrd<$nonzero> for $real {
                fn partial_cmp(&self, other: &$nonzero) -> Option<Ordering> {
                    PartialOrd::partial_cmp(&self.0, &(other.get() as $float))
                }
            }
        )+
    };
}

macro_rules! real_from_signed_ints_impl {
    ($real:ty { $float:ty } : $($nonzero:ty{ $int:ty }),+) => {
        $(
            impl From<$int> for $real {
                fn from(x: $int) -> Self {Self(x as $float)}
            }

            impl IntoPropValue<$real> for $int {
                fn into_prop_value(self) -> $real {self.into()}
            }

            impl From<$real> for $int {
                fn from(x: $real) -> Self {
                    if      *x >= <$int>::MAX as $float {<$int>::MAX}
                    else if *x <= <$int>::MIN as $float {<$int>::MIN}
                    else {*x as $int}
                }
            }

            impl IntoPropValue<$int> for $real {
                fn into_prop_value(self) -> $int {self.into()}
            }

            impl PartialEq<$int> for $real {
                fn eq(&self, other: &$int) -> bool {
                    PartialEq::eq(&self.0, &(*other as $float))
                }
            }

            impl PartialOrd<$int> for $real {
                fn partial_cmp(&self, other: &$int) -> Option<Ordering> {
                    PartialOrd::partial_cmp(&self.0, &(*other as $float))
                }
            }

            impl From<$nonzero> for $real {
                fn from(x: $nonzero) -> Self {Self(x.get() as $float)}
            }

            impl IntoPropValue<$real> for $nonzero {
                fn into_prop_value(self) -> $real {self.into()}
            }

            impl TryFrom<$real> for $nonzero {
                type Error = TryFromIntError;
                fn try_from(x: $real) -> Result<Self, Self::Error> {
                    <$nonzero>::try_from(<$int>::from(x))
                }
            }

            impl PartialEq<$nonzero> for $real {
                fn eq(&self, other: &$nonzero) -> bool {
                    PartialEq::eq(&self.0, &(other.get() as $float))
                }
            }

            impl PartialOrd<$nonzero> for $real {
                fn partial_cmp(&self, other: &$nonzero) -> Option<Ordering> {
                    PartialOrd::partial_cmp(&self.0, &(other.get() as $float))
                }
            }
        )+
    };
}

macro_rules! impl_op {
    ($op:ident :: $method:ident ($self:ident : $real:ty , $rhs:ident : $rhs_ty:ty) -> $out:ty { $($s:stmt);+ }) => {
        impl $op<$rhs_ty> for $real {
            type Output = $out;
            fn $method($self, $rhs: $rhs_ty) -> Self::Output { $($s)+ }
        }

        impl $op<&$rhs_ty> for $real {
            type Output = $out;
            fn $method(self, rhs: &$rhs_ty) -> Self::Output {$op::$method(self, *rhs)}
        }

        impl<'a> $op<$rhs_ty> for &'a $real {
            type Output = $out;
            fn $method(self, rhs: $rhs_ty) -> Self::Output {$op::$method(*self, rhs)}
        }

        impl<'a> $op<&$rhs_ty> for &'a $real {
            type Output = $out;
            fn $method(self, rhs: &$rhs_ty) -> Self::Output {$op::$method(*self, *rhs)}
        }
    };
}

macro_rules! impl_assign_op {
    ($op:ident :: $method:ident ($self:ident : $real:ty , $rhs:ident : $rhs_ty:ty) { $($s:stmt);+ }) => {
        impl $op<$rhs_ty> for $real {
            fn $method(&mut $self, $rhs: $rhs_ty) { $($s)+ }
        }

        impl $op<&$rhs_ty> for $real {
            fn $method(&mut self, rhs: &$rhs_ty) {$op::$method(self, *rhs)}
        }
    };
}

macro_rules! real_float_operator_impl {
    ($real:ty { $float:ty } , $other_float:ty : $($op:ident :: $method:ident | $assign_op:ident :: $assign_method:ident),+) => {
        $(
            impl_op!($op::$method(self: $real, rhs: $other_float) -> $real {
                let res = self.0.$method(rhs as $float);
                if res.is_nan() {report_err(js_sys::Error::new(&format!("{self} {} {rhs} = NaN", stringify!($method)))); self}
                else {Self(res)}
            });

            impl_assign_op!($assign_op::$assign_method(self: $real, rhs: $other_float) {
                let res = self.0.$method(rhs as $float);
                if res.is_nan() {report_err(js_sys::Error::new(&format!("{self} {} {rhs} = NaN", stringify!($method))))}
                else {self.0 = res}
            });
        )+
    }
}

macro_rules! real_int_operator_impl {
    (infallible $op:ident :: $method:ident | $assign_op:ident :: $assign_method:ident for $real:ty { $float:ty } and $($nonzero:ty { $int:ty } ),+) => {
        $(
            impl_op!($op::$method(self: $real, rhs: $int) -> $real {
                Self(self.0.$method(rhs as $float))
            });

            impl_assign_op!($assign_op::$assign_method(self: $real, rhs: $int) {
                self.0.$assign_method(rhs as $float)
            });

            impl_op!($op::$method(self: $real, rhs: $nonzero) -> $real {
                Self(self.0.$method(rhs.get() as $float))
            });

            impl_assign_op!($assign_op::$assign_method(self: $real, rhs: $nonzero) {
                self.0.$assign_method(rhs.get() as $float)
            });
        )+
    };

    (fallible $op:ident :: $method:ident | $assign_op:ident :: $assign_method:ident for $real:ty { $float:ty } and $($nonzero:ty { $int:ty } ),+) => {
        $(
            impl_op!($op::$method(self: $real, rhs: $int) -> $real {
                let res = self.0.$method(rhs as $float);
                if res.is_nan() {report_err(js_sys::Error::new(&format!("{self} {} {rhs} = NaN", stringify!($method)))); self}
                else {Self(res)}
            });

            impl_assign_op!($assign_op::$assign_method(self: $real, rhs: $int) {
                let res = self.0.$method(rhs as $float);
                if res.is_nan() {report_err(js_sys::Error::new(&format!("{self} {} {rhs} = NaN", stringify!($method))))}
                else {self.0 = res}
            });

            impl_op!($op::$method(self: $real, rhs: $nonzero) -> $real {
                Self(self.0.$method(rhs.get() as $float))
            });

            impl_assign_op!($assign_op::$assign_method(self: $real, rhs: $nonzero) {
                self.0.$assign_method(rhs.get() as $float)
            });
        )+
    };
}

macro_rules! real_real_operator_impl {
    ($real:ty { $float:ty } , $other_real:ty { $other_float:ty } : $($op:ident :: $method:ident | $assign_op:ident :: $assign_method:ident),+) => {
        $(
            impl_op!($op::$method(self: $real, rhs: $other_real) -> $real {
                let res = self.0.$method(rhs.0 as $float);
                if res.is_nan() {report_err(js_sys::Error::new(&format!("{self} {} {rhs} = NaN", stringify!($method)))); self}
                else {Self(res)}
            });

            impl_assign_op!($assign_op::$assign_method(self: $real, rhs: $other_real) {
                let res = self.0.$method(rhs.0 as $float);
                if res.is_nan() {report_err(js_sys::Error::new(&format!("{self} {} {rhs} = NaN", stringify!($method))))}
                else {self.0 = res}
            });
        )+
    }
}

macro_rules! real_impl {
    ($real:ident { $float:ident }, $other_real:ty { $other_float:ty }) => {
        #[derive(Debug, Default, PartialEq, Clone, Copy)]
        pub struct $real($float);

        impl Deref for $real {
            type Target = $float;
            fn deref(&self) -> &Self::Target {&self.0}
        }

        impl PartialEq<$float> for $real {
            fn eq(&self, other: &$float) -> bool {
                self.0.eq(other)
            }
        }

        impl PartialOrd<$float> for $real {
            fn partial_cmp(&self, other: &$float) -> Option<Ordering> {
                self.0.partial_cmp(&other)
            }
        }

        impl PartialOrd for $real {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        impl Ord for $real {
            fn cmp(&self, other: &Self) -> Ordering {
                unsafe{self.0.partial_cmp(&other.0).unwrap_unchecked()}
            }
        }

        impl Eq for $real {}

        impl TryFrom<$float> for $real {
            type Error = AppError;
            fn try_from(x: $float) -> Result<Self, Self::Error> {
                Self::new(x).ok_or(app_error!("the value is NaN"))
            }
        }

        impl From<$other_real> for $real {
            fn from(x: $other_real) -> Self {Self(x.0 as $float)}
        }

        impl IntoPropValue<$other_real> for $real {
            fn into_prop_value(self) -> $other_real {self.into()}
        }

        impl Display for $real {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                Display::fmt(&self.0, f)
            }
        }

        impl Neg for $real {
            type Output = Self;
            fn neg(self) -> Self::Output {Self(-self.0)}
        }

        impl RoundTo for $real {
            fn floor_to(self, step: Self) -> Self {
                if self.is_infinite() || step == 0 {return self}
                Self(*self - *self % *step)
            }

            fn  ceil_to(self, step: Self) -> Self {
                if self.is_infinite() || step == 0 {return self}
                let prev = *self - 1.0;
                Self(*step - prev % *step + prev)
            }
        }

        impl Sum for $real {
            fn sum<I>(iter: I) -> Self where I: Iterator<Item=$real> {
                iter.fold(Self(0.0), |s, n| s + n)
            }
        }

        impl<'a> Sum<&'a $real> for $real {
            fn sum<I>(iter: I) -> Self where I: Iterator<Item=&'a $real> {
                iter.fold(Self(0.0), |s, n| s + n)
            }
        }

        real_from_unsigned_ints_impl!($real{$float}:
            NonZeroU8{u8}, NonZeroU16{u16}, NonZeroU32{u32}, NonZeroUsize{usize}, NonZeroU64{u64});
        real_from_signed_ints_impl!($real{$float}:
            NonZeroI8{i8}, NonZeroI16{i16}, NonZeroI32{i32}, NonZeroIsize{isize}, NonZeroI64{i64});

        real_int_operator_impl!(infallible Add::add|AddAssign::add_assign for $real{$float} and
            NonZeroU8{u8}, NonZeroU16{u16}, NonZeroU32{u32}, NonZeroUsize{usize}, NonZeroU64{u64},
            NonZeroI8{i8}, NonZeroI16{i16}, NonZeroI32{i32}, NonZeroIsize{isize}, NonZeroI64{i64});
        real_int_operator_impl!(infallible Sub::sub|SubAssign::sub_assign for $real{$float} and
            NonZeroU8{u8}, NonZeroU16{u16}, NonZeroU32{u32}, NonZeroUsize{usize}, NonZeroU64{u64},
            NonZeroI8{i8}, NonZeroI16{i16}, NonZeroI32{i32}, NonZeroIsize{isize}, NonZeroI64{i64});
        real_int_operator_impl!(fallible Mul::mul|MulAssign::mul_assign for $real{$float} and
            NonZeroU8{u8}, NonZeroU16{u16}, NonZeroU32{u32}, NonZeroUsize{usize}, NonZeroU64{u64},
            NonZeroI8{i8}, NonZeroI16{i16}, NonZeroI32{i32}, NonZeroIsize{isize}, NonZeroI64{i64});
        real_int_operator_impl!(fallible Div::div|DivAssign::div_assign for $real{$float} and
            NonZeroU8{u8}, NonZeroU16{u16}, NonZeroU32{u32}, NonZeroUsize{usize}, NonZeroU64{u64},
            NonZeroI8{i8}, NonZeroI16{i16}, NonZeroI32{i32}, NonZeroIsize{isize}, NonZeroI64{i64});
        real_int_operator_impl!(fallible Rem::rem|RemAssign::rem_assign for $real{$float} and
            NonZeroU8{u8}, NonZeroU16{u16}, NonZeroU32{u32}, NonZeroUsize{usize}, NonZeroU64{u64},
            NonZeroI8{i8}, NonZeroI16{i16}, NonZeroI32{i32}, NonZeroIsize{isize}, NonZeroI64{i64});

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

            pub const fn new(x: $float) -> Option<Self> {
                if x.is_nan() {None} else {Some(Self(x))}
            }

            /// # Safety
            /// `x` must not be NaN
            pub const unsafe fn new_unchecked(x: $float) -> Self {Self(x)}

            pub const fn new_or(default: Self, x: $float) -> Self {
                if x.is_nan() {default} else {Self(x)}
            }

            pub fn rem_euclid(self, rhs: Self) -> Option<Self> {
                let res = self.0.rem_euclid(rhs.0);
                if res.is_nan() {return None}
                Some(Self(res))
            }

            pub fn copysign(self, sign: Self) -> Self {
                Self(self.0.copysign(*sign))
            }

            pub fn recip(self) -> Self {Self(self.0.recip())}

            pub fn exp2(self) -> Self {Self(self.0.exp2())}

            pub fn floor(self) -> Self {Self(self.0.floor())}

            pub fn ceil(self) -> Self {Self(self.0.ceil())}

            pub fn round(self) -> Self {Self(self.0.round())}

            pub fn is_finite(&self) -> bool {self.0.is_finite()}

            pub fn abs(self) -> Self {Self(self.0.abs())}

            pub fn sin(self) -> Option<Self> {Self::new(self.0.sin())}

            /// # Safety
            /// `self` must be finite
            pub unsafe fn sin_unchecked(self) -> Self {Self(self.0.sin())}

            pub fn sin_or(self, default: Self) -> Self {
                Self::new(self.0.sin()).unwrap_or(default)
            }

            pub fn cos(self) -> Option<Self> {Self::new(self.0.cos())}

            /// # Safety
            /// `self` must be finite
            pub unsafe fn cos_unchecked(self) -> Self {Self(self.0.cos())}

            pub fn cos_or(self, default: Self) -> Self {
                Self::new(self.0.cos()).unwrap_or(default)
            }
        }
    };
}

real_impl!(R32{f32}, R64{f64});
real_impl!(R64{f64}, R32{f32});

#[macro_export]
macro_rules! r32 {
    ($x:literal) => {{
        #[allow(unused_unsafe)]
        unsafe{$crate::R32::new_unchecked($x as f32)}
    }};

    ($x:expr) => {{
        #[allow(unused_unsafe)]
        unsafe{$crate::R64::new_unchecked(($x + 0) as f64)}
    }}
}

#[macro_export]
macro_rules! r64 {
    ($x:literal) => {{
        #[allow(unused_unsafe)]
        unsafe{$crate::R64::new_unchecked($x as f64)}
    }};

    ($x:expr) => {{
        #[allow(unused_unsafe)]
        unsafe{$crate::R64::new_unchecked(($x + 0) as f64)}
    }}
}

#[macro_export]
macro_rules! const_assert {
    ($x:expr $(,)?) => {
        #[allow(unknown_lints, eq_op)]
        const _: [(); 0 - !{ const ASSERT: bool = $x; ASSERT } as usize] = [];
    };
}
