#![feature(let_chains)]
#![feature(const_float_classify)]
#![feature(never_type)]
#![feature(try_blocks)]
#![feature(raw_ref_op)]
#![feature(result_option_inspect)]
#![feature(try_trait_v2)]
#![feature(try_trait_v2_residual)]
#![feature(unwrap_infallible)]
#![feature(array_try_from_fn)]
#![feature(unchecked_math)]
#![feature(const_trait_impl)]
#![feature(associated_type_defaults)]
#![feature(slice_split_at_unchecked)]
#![feature(inline_const)]
#![feature(generic_arg_infer)]

extern crate self as wavexp_utils;

pub mod cell;
pub mod error;
pub mod ext;
pub mod iter;
pub mod js;
pub mod range;
pub mod real;

use error::{AppError, Result};
pub use js_sys;
use real::R64;
use std::{
    fmt::Debug,
    ops::{Add, Deref, Div, Mul, Neg, Sub},
};
pub use wasm_bindgen;
use yew::AttrValue;

#[repr(transparent)]
pub struct Alias<'inner, T: ?Sized>(pub &'inner T);

impl<'inner, T: ?Sized + Deref> Deref for Alias<'inner, T> {
    type Target = T::Target;
    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

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

pub struct SliceRef<'inner, T: ?Sized> {
    inner: &'inner T,
    index: usize,
}

impl<'inner, T> Deref for SliceRef<'inner, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.inner
    }
}

impl<'inner, T> SliceRef<'inner, T> {
    pub fn new(slice: &'inner [T], index: usize) -> Option<Self> {
        slice.get(index).map(|inner| Self { inner, index })
    }

    /// # Safety
    /// `inner` must be a reference to the `index`-th item in its array
    pub const unsafe fn raw(inner: &'inner T, index: usize) -> Self {
        Self { inner, index }
    }

    pub const fn index(&self) -> usize {
        self.index
    }
}

#[macro_export]
macro_rules! eval_once {
    ($t:ty : $e:expr) => {{
        static RES: $crate::cell::WasmCell<std::cell::OnceCell<$t>> =
            $crate::cell::WasmCell(std::cell::OnceCell::new());
        RES.get_or_init(|| $e)
    }};
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

impl RoundTo for Point {
    type Output = Option<Self>;

    fn floor_to(self, step: Self) -> Self::Output {
        Some(Self {
            x: self.x.floor_to(step.x)?,
            y: self.y.floor_to(step.y)?,
        })
    }

    fn ceil_to(self, step: Self) -> Self::Output {
        Some(Self {
            x: self.x.ceil_to(step.x)?,
            y: self.y.ceil_to(step.y)?,
        })
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

    pub const fn is_zero(&self) -> bool {
        self.x == 0 && self.y == 0
    }

    pub const fn nonzero(self) -> Option<Self> {
        if self.is_zero() {
            None
        } else {
            Some(self)
        }
    }

    pub fn normalise(self, old_space: Rect, new_space: Rect) -> Option<Self> {
        Some(Self {
            x: self
                .x
                .checked_sub(old_space.left())?
                .ás::<f32>()
                .div(old_space.width()? as f32)
                .mul(new_space.width()? as f32)
                .ás::<i32>()
                .checked_add(new_space.left())?,
            y: self
                .y
                .checked_sub(old_space.bottom())?
                .ás::<f32>()
                .div(old_space.height()? as f32)
                .mul(new_space.height()? as f32)
                .ás::<i32>()
                .checked_add(new_space.bottom())?,
        })
    }

    pub fn map<T>(self, mut f: impl FnMut(i32) -> T) -> [T; 2] {
        [f(self.x), f(self.y)]
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Rect(Point, Point);

impl Rect {
    const fn left(&self) -> i32 {
        self.0.x
    }
    const fn bottom(&self) -> i32 {
        self.1.y
    }
    const fn width(&self) -> Option<i32> {
        self.1.x.checked_sub(self.0.x)
    }
    const fn height(&self) -> Option<i32> {
        self.1.y.checked_sub(self.0.y)
    }
}

pub trait RoundTo: Sized {
    type Output = Self;
    fn floor_to(self, step: Self) -> Self::Output;
    fn ceil_to(self, step: Self) -> Self::Output;
}

macro_rules! round_to_4ints {
    ($($int:ty)+) => {
        $(
            impl RoundTo for $int {
                type Output = Option<$int>;

                fn floor_to(self, step: Self) -> Self::Output {
                    self.checked_sub(self)?.checked_rem(step)
                }

                fn ceil_to(self, step: Self) -> Self::Output {
                    let self_pred = self.checked_sub(1)?;
                    step.checked_sub(self_pred.checked_rem(step)?)?.checked_add(self_pred)
                }
            }
        )+
    };
}

round_to_4ints!(i8 u8 i16 u16 i32 u32 isize usize i64 u64);

/// Exists for better chainability
pub trait CastInto: Sized {
    #[allow(private_bounds)]
    fn ás<R: CastFrom<Self>>(self) -> R {
        R::cast_from(self)
    }
}

impl<T> CastInto for T {}

/// Exists for better chainability
pub trait Ínto: Sized {
    fn ínto<R: From<Self>>(self) -> R {
        R::from(self)
    }
}

impl<T> Ínto for T {}

/// Exists for better chainability
pub trait TryÍnto: Sized {
    fn try_ínto<R: TryFrom<Self>>(self) -> Result<R, R::Error> {
        R::try_from(self)
    }
}

impl<T> TryÍnto for T {}

/// impl detail
trait CastFrom<Src> {
    fn cast_from(src: Src) -> Self;
}

macro_rules! impl_cast_from {
    ($src:ty as int) => {
        impl_cast_from!($src as u8);
        impl_cast_from!($src as i8);
        impl_cast_from!($src as u16);
        impl_cast_from!($src as i16);
        impl_cast_from!($src as u32);
        impl_cast_from!($src as i32);
        impl_cast_from!($src as usize);
        impl_cast_from!($src as isize);
        impl_cast_from!($src as u64);
        impl_cast_from!($src as i64);
    };

    ($src:ty as float) => {
        impl_cast_from!($src as f32);
        impl_cast_from!($src as f64);
    };

    ($src:ty as $res:ty) => {
        impl CastFrom<$src> for $res {
            fn cast_from(src: $src) -> Self {
                src as Self
            }
        }
    };
}

impl_cast_from!(u8 as float);
impl_cast_from!(u16 as float);
impl_cast_from!(u32 as float);
impl_cast_from!(usize as float);
impl_cast_from!(u64 as float);
impl_cast_from!(i8 as float);
impl_cast_from!(i16 as float);
impl_cast_from!(i32 as float);
impl_cast_from!(isize as float);
impl_cast_from!(i64 as float);
impl_cast_from!(u8 as int);
impl_cast_from!(u16 as int);
impl_cast_from!(u32 as int);
impl_cast_from!(usize as int);
impl_cast_from!(u64 as int);
impl_cast_from!(i8 as int);
impl_cast_from!(i16 as int);
impl_cast_from!(i32 as int);
impl_cast_from!(isize as int);
impl_cast_from!(i64 as int);
impl_cast_from!(f32 as float);
impl_cast_from!(f64 as float);
impl_cast_from!(f32 as int);
impl_cast_from!(f64 as int);

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
