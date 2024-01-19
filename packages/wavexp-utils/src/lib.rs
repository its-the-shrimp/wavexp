#![feature(let_chains)]
#![feature(const_float_classify)]
#![feature(never_type)]
#![feature(try_blocks)]
#![feature(raw_ref_op)]
#![feature(result_option_inspect)]

pub mod cell;
pub mod ext;
pub mod iter;

use ext::HtmlDocumentExt;
pub use js_sys;
use std::{
    cmp::Ordering,
    fmt::{self, Debug, Display, Formatter},
    iter::Sum,
    mem::{forget, transmute_copy, MaybeUninit},
    num::{
        NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI8, NonZeroIsize, NonZeroU16, NonZeroU32,
        NonZeroU64, NonZeroU8, NonZeroUsize, TryFromIntError,
    },
    ops::{
        Add, AddAssign, Deref, Div, DivAssign, Mul, MulAssign, Neg, RangeBounds, Rem, RemAssign,
        Sub, SubAssign,
    },
};
pub use wasm_bindgen;
use wasm_bindgen::{JsCast, JsValue};
use web_sys::{
    console::{warn_1, warn_2},
    Document, HtmlElement, Window,
};
use yew::{html::IntoPropValue, AttrValue};

pub fn default<T: Default>() -> T {
    T::default()
}

#[repr(transparent)]
pub struct Alias<'a, T: ?Sized>(pub &'a T);

impl<'a, T: ?Sized + Deref> Deref for Alias<'a, T> {
    type Target = T::Target;
    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

pub trait Check: Sized {
    fn check(self, f: impl FnOnce(&Self) -> bool) -> Result<Self, Self> {
        if f(&self) {
            Ok(self)
        } else {
            Err(self)
        }
    }

    fn check_in<R>(self, range: R) -> Result<Self, Self>
    where
        Self: PartialOrd,
        R: RangeBounds<Self>,
    {
        if range.contains(&self) {
            Ok(self)
        } else {
            Err(self)
        }
    }

    fn check_not_in<R>(self, range: R) -> Result<Self, Self>
    where
        Self: PartialOrd,
        R: RangeBounds<Self>,
    {
        if !range.contains(&self) {
            Ok(self)
        } else {
            Err(self)
        }
    }
}
impl<T> Check for T {}

pub trait Tee: Sized {
    fn tee(self, f: impl FnOnce(&Self)) -> Self {
        f(&self);
        self
    }

    fn js_log(self, label: &str) -> Self
    where
        Self: Debug,
    {
        js_log!("{}{:?}", label, &self);
        self
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
        if cond {
            f(self)
        } else {
            self
        }
    }
}
impl<T> Pipe for T {}

pub trait ArrayFrom<T, const N: usize>: Sized {
    fn array_from(x: T) -> [Self; N];
}

impl<S, D, const N: usize> ArrayFrom<[S; N], N> for D
where
    D: From<S>,
{
    fn array_from(x: [S; N]) -> [Self; N] {
        x.map(D::from)
    }
}

pub trait IntoArray<T, const N: usize> {
    fn into_array(self) -> [T; N];
}

impl<T, U, const N: usize> IntoArray<U, N> for T
where
    U: ArrayFrom<T, N>,
{
    fn into_array(self) -> [U; N] {
        U::array_from(self)
    }
}

pub trait FlippedArray<T, const OUTER: usize, const INNER: usize> {
    fn flipped(self) -> [[T; OUTER]; INNER];
}

impl<T, const OUTER: usize, const INNER: usize> FlippedArray<T, OUTER, INNER>
    for [[T; INNER]; OUTER]
{
    fn flipped(mut self) -> [[T; OUTER]; INNER] {
        unsafe {
            if OUTER == INNER {
                let mut src = self.as_mut_ptr() as *mut T;
                for outer in 0..OUTER {
                    src = src.add(outer + 1);
                    for inner in outer + 1..INNER {
                        src.swap(src.add((inner - outer) * (INNER - 1)));
                        src = src.add(1);
                    }
                }
                transmute_copy(&self)
            } else {
                let mut new_self: MaybeUninit<_> = MaybeUninit::uninit();
                let mut res = new_self.as_mut_ptr() as *mut T;
                for inner in 0..INNER {
                    let mut src = (self.as_mut_ptr() as *mut T).add(inner);
                    for _ in 0..OUTER {
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
    index: usize,
}

impl<'a, T> Deref for SliceRef<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.inner
    }
}

impl<'a, T> SliceRef<'a, T> {
    pub fn new(slice: &'a [T], index: usize) -> Option<Self> {
        slice.get(index).map(|inner| Self { inner, index })
    }

    /// # Safety
    /// `inner` must be a reference to the `index`-th item in its array
    pub unsafe fn raw(inner: &'a T, index: usize) -> Self {
        Self { inner, index }
    }

    pub fn index(&self) -> usize {
        self.index
    }
}

#[allow(non_camel_case_types)]
#[allow(dead_code)]
pub mod js_types {
    use js_sys::{Boolean as JsBoolean, JsString, Number as JsNumber};
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
    ($obj:ident . $method:ident) => {
        $crate::wasm_bindgen::closure::Closure::<dyn FnMut(_)>::new(move |x| $obj.$method(x))
            .into_js_value()
            .unchecked_into::<$crate::js_sys::Function>()
    }
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
        static RES: $crate::cell::WasmCell<std::cell::OnceCell<$t>> =
            $crate::cell::WasmCell(std::cell::OnceCell::new());
        RES.get_or_init(|| $e)
    }};
}

pub fn window() -> Window {
    unsafe { web_sys::window().unwrap_unchecked() }
}

pub fn document() -> Document {
    unsafe {
        web_sys::window()
            .unwrap_unchecked()
            .document()
            .unwrap_unchecked()
    }
}

/// returns precise current time in seconds.
pub fn now() -> Option<R64> {
    window()
        .performance()
        .map(|p| unsafe { R64::new_unchecked(p.now()) } / 1000)
}

pub fn report_err(err: js_sys::Error) {
    warn_2(
        &err,
        &js_sys::Reflect::get(err.as_ref(), &"stack".into()).unwrap_or_else(|e| e),
    );
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
            Err(x) => Self::new(&String::from(js_sys::Object::from(x).to_string())),
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
    fn from(value: AppError) -> Self {
        value.0
    }
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
            Err(e) => {
                report_err(e.into());
                None
            }
        }
    }
}

pub trait LooseEq<O = Self> {
    fn loose_eq(&self, value: Self, off: O) -> bool;
    fn loose_ne(&self, value: Self, off: O) -> bool
    where
        Self: Sized,
    {
        !self.loose_eq(value, off)
    }
}

impl<O, T> LooseEq<O> for T
where
    O: Copy,
    T: PartialOrd + Add<O, Output = Self> + Sub<O, Output = Self> + Copy,
{
    fn loose_eq(&self, value: Self, off: O) -> bool {
        (value - off..value + off).contains(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Point {
    pub x: i32,
    pub y: i32,
}

impl From<[R64; 2]> for Point {
    fn from(value: [R64; 2]) -> Self {
        Self {
            x: value[0].into(),
            y: value[1].into(),
        }
    }
}

impl<D> ArrayFrom<Point, 2> for D
where
    D: From<i32>,
{
    fn array_from(x: Point) -> [Self; 2] {
        [x.x.into(), x.y.into()]
    }
}

impl LooseEq for Point {
    fn loose_eq(&self, value: Self, off: Self) -> bool {
        self.x.loose_eq(value.x, off.x) && self.y.loose_eq(value.y, off.y)
    }
}

impl RoundTo for Point {
    fn floor_to(self, step: Self) -> Self {
        self.x.floor_to(step.x);
        self.y.floor_to(step.y);
        self
    }

    fn ceil_to(self, step: Self) -> Self {
        self.x.ceil_to(step.x);
        self.y.ceil_to(step.y);
        self
    }
}

impl Add for Point {
    type Output = Option<Self>;
    fn add(self, rhs: Self) -> Self::Output {
        Some(Self {
            x: self.x.checked_add(rhs.x)?,
            y: self.y.checked_add(rhs.y)?,
        })
    }
}

impl Sub for Point {
    type Output = Option<Self>;

    fn sub(self, rhs: Self) -> Self::Output {
        Some(Self {
            x: self.x.checked_sub(rhs.x)?,
            y: self.y.checked_sub(rhs.y)?,
        })
    }
}

impl Neg for Point {
    type Output = Option<Point>;

    fn neg(self) -> Self::Output {
        Some(Self {
            x: self.x.checked_neg()?,
            y: self.y.checked_neg()?,
        })
    }
}

impl Point {
    pub const ZERO: Self = Self { x: 0, y: 0 };

    pub fn is_zero(&self) -> bool {
        self.x == 0 && self.y == 0
    }

    pub fn nonzero(self) -> Option<Self> {
        if self.is_zero() {
            None
        } else {
            Some(self)
        }
    }

    pub fn normalise(mut self, old_space: Rect, new_space: Rect) -> Self {
        self.y = (((self.y - old_space.bottom()) as f32 / old_space.height() as f32)
            * new_space.height() as f32) as i32
            + new_space.bottom();
        self.x = (((self.x - old_space.left()) as f32 / old_space.width() as f32)
            * new_space.width() as f32) as i32
            + new_space.left();
        self
    }

    pub fn map<T>(self, mut f: impl FnMut(i32) -> T) -> [T; 2] {
        [f(self.x), f(self.y)]
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Rect(Point, Point);

impl Rect {
    fn left(&self) -> i32 {
        self.0.x
    }
    fn bottom(&self) -> i32 {
        self.1.y
    }
    fn width(&self) -> i32 {
        self.1.x - self.0.x
    }
    fn height(&self) -> i32 {
        self.1.y - self.0.y
    }
}

pub trait RoundTo {
    fn floor_to(self, step: Self) -> Self;
    fn ceil_to(self, step: Self) -> Self;
}

macro_rules! round_to_4ints {
    ($($int:ty)+) => {$(
        impl RoundTo for $int {
            fn floor_to(self, step: Self) -> Self {
                self - self % step
            }

            fn ceil_to(self, step: Self) -> Self {
                step - (self - 1) % step + self - 1
            }
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
                // Safety: the contained floats are guaranteed to never be NaN
                unsafe { self.0.partial_cmp(&other.0).unwrap_unchecked() }
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
                if self.is_infinite() || step == 0 { return self }
                Self(*self - *self % *step)
            }

            fn ceil_to(self, step: Self) -> Self {
                if self.is_infinite() || step == 0 { return self }
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

real_impl!(R32 { f32 }, R64 { f64 });
real_impl!(R64 { f64 }, R32 { f32 });

#[macro_export]
macro_rules! r32 {
    ($x:literal) => {{
        #[allow(unused_unsafe)]
        unsafe {
            $crate::R32::new_unchecked($x as f32)
        }
    }};

    ($x:expr) => {{
        #[allow(unused_unsafe)]
        unsafe {
            $crate::R64::new_unchecked(($x + 0) as f64)
        }
    }};
}

#[macro_export]
macro_rules! r64 {
    ($x:literal) => {{
        #[allow(unused_unsafe)]
        unsafe {
            $crate::R64::new_unchecked($x as f64)
        }
    }};

    ($x:expr) => {{
        #[allow(unused_unsafe)]
        unsafe {
            $crate::R64::new_unchecked(($x + 0) as f64)
        }
    }};
}

#[macro_export]
macro_rules! const_assert {
    ($x:expr $(,)?) => {
        #[allow(unknown_lints, eq_op)]
        const _: [(); 0 - !{
            const ASSERT: bool = $x;
            ASSERT
        } as usize] = [];
    };
}
