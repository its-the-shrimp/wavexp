use crate::ext::add;
use crate::{
    app_error,
    error::{AppError, Result},
    RoundTo,
};
pub use js_sys;
use std::{
    cmp::Ordering,
    fmt::{self, Debug, Display, Formatter},
    iter::Sum,
    num::{
        NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI8, NonZeroIsize, NonZeroU16, NonZeroU32,
        NonZeroU64, NonZeroU8, NonZeroUsize, TryFromIntError,
    },
    ops::{
        Add, AddAssign, Deref, Div, DivAssign, Mul, MulAssign, Neg, Rem, RemAssign, Sub, SubAssign,
    },
};
pub use wasm_bindgen;
use yew::html::IntoPropValue;

macro_rules! real_from_unsigned_ints_impl {
    ($real:ty { $float:ty } : $($nonzero:ty{ $int:ty }),+) => {
        $(
            impl From<$int> for $real {
                fn from(x: $int) -> Self {
                    Self(x as $float)
                }
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
    {
        fn $op:ident::$method:ident($self:ident: $real:ty, $rhs:ident: $rhs_ty:ty) -> $out:ty $body:block
    } => {
        impl $op<$rhs_ty> for $real {
            type Output = $out;
            fn $method($self, $rhs: $rhs_ty) -> Self::Output $body
        }

        impl $op<&$rhs_ty> for $real {
            type Output = $out;
            fn $method(self, rhs: &$rhs_ty) -> Self::Output {$op::$method(self, *rhs)}
        }

        impl<'this> $op<$rhs_ty> for &'this $real {
            type Output = $out;
            fn $method(self, rhs: $rhs_ty) -> Self::Output {$op::$method(*self, rhs)}
        }

        impl<'this> $op<&$rhs_ty> for &'this $real {
            type Output = $out;
            fn $method(self, rhs: &$rhs_ty) -> Self::Output {$op::$method(*self, *rhs)}
        }
    };
}

macro_rules! impl_assign_op {
    {
        fn $op:ident::$method:ident($self:ident: $real:ty, $rhs:ident: $rhs_ty:ty) $body:block
    } => {
        impl $op<$rhs_ty> for $real {
            fn $method(&mut $self, $rhs: $rhs_ty) $body
        }

        impl $op<&$rhs_ty> for $real {
            fn $method(&mut self, rhs: &$rhs_ty) {$op::$method(self, *rhs)}
        }
    };
}

macro_rules! real_float_operator_impl {
    ($real:ty { $float:ty } , $other_float:ty : $($op:ident :: $method:ident | $assign_op:ident :: $assign_method:ident),+) => {
        $(
            impl_op! {
                fn $op::$method(self: $real, rhs: $other_float) -> $real {
                    Self::new(self.0.$method(rhs as $float))
                        .unwrap_or_else(|| <$real>::INFINITY.copysign(self))
                }
            }

            impl_assign_op! {
                fn $assign_op::$assign_method(self: $real, rhs: $other_float) {
                    *self = self.$method(rhs)
                }
            }
        )+
    }
}

macro_rules! real_int_operator_impl {
    (infallible $op:ident :: $method:ident | $assign_op:ident :: $assign_method:ident for $real:ty { $float:ty } and $($nonzero:ty { $int:ty } ),+) => {
        $(
            impl_op! {
                fn $op::$method(self: $real, rhs: $int) -> $real {
                    Self(self.0.$method(rhs as $float))
                }
            }

            impl_assign_op! {
                fn $assign_op::$assign_method(self: $real, rhs: $int) {
                    self.0.$assign_method(rhs as $float)
                }
            }

            impl_op! {
                fn $op::$method(self: $real, rhs: $nonzero) -> $real {
                    Self(self.0.$method(rhs.get() as $float))
                }
            }

            impl_assign_op! {
                fn $assign_op::$assign_method(self: $real, rhs: $nonzero) {
                    self.0.$assign_method(rhs.get() as $float)
                }
            }
        )+
    };

    (fallible $op:ident :: $method:ident | $assign_op:ident :: $assign_method:ident for $real:ty { $float:ty } and $($nonzero:ty { $int:ty } ),+) => {
        $(
            impl_op! {
                fn $op::$method(self: $real, rhs: $int) -> $real {
                    Self::new(self.0.$method(rhs as $float))
                        .unwrap_or_else(|| <$real>::INFINITY.copysign(self))
                }
            }

            impl_assign_op! {
                fn $assign_op::$assign_method(self: $real, rhs: $int) {
                    *self = self.$method(rhs)
                }
            }

            impl_op! {
                fn $op::$method(self: $real, rhs: $nonzero) -> $real {
                    self.$method(rhs.get())
                }
            }

            impl_assign_op! {
                fn $assign_op::$assign_method(self: $real, rhs: $nonzero) {
                    *self = self.$method(rhs)
                }
            }
        )+
    };
}

macro_rules! real_real_operator_impl {
    ($real:ty { $float:ty } , $other_real:ty { $other_float:ty } : $($op:ident :: $method:ident | $assign_op:ident :: $assign_method:ident),+) => {
        $(
            impl_op! {
                fn $op::$method(self: $real, rhs: $other_real) -> $real {
                    Self::new(self.0.$method(rhs.0 as $float))
                        .unwrap_or_else(|| <$real>::INFINITY.copysign(self))
                }
            }

            impl_assign_op! {
                fn $assign_op::$assign_method(self: $real, rhs: $other_real) {
                    *self = self.$method(rhs)
                }
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
            fn deref(&self) -> &Self::Target {
                &self.0
            }
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
                Self::new(x).ok_or_else(|| app_error!("the value is NaN"))
            }
        }

        impl From<$other_real> for $real {
            fn from(x: $other_real) -> Self {
                Self(x.0 as $float)
            }
        }

        impl IntoPropValue<$other_real> for $real {
            fn into_prop_value(self) -> $other_real {
                self.into()
            }
        }

        impl Display for $real {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                Display::fmt(&self.0, f)
            }
        }

        impl Neg for $real {
            type Output = Self;
            fn neg(self) -> Self::Output {
                Self(-self.0)
            }
        }

        impl RoundTo for $real {
            fn floor_to(self, step: Self) -> Self {
                if self.is_infinite() || step == 0 {
                    return self;
                }
                Self(*self - *self % *step)
            }

            fn ceil_to(self, step: Self) -> Self {
                if self.is_infinite() || step == 0 {
                    return self;
                }
                let prev = *self - 1.0;
                Self(*step - prev % *step + prev)
            }
        }

        impl Sum<$real> for $real {
            fn sum<I>(iter: I) -> Self
            where
                I: Iterator<Item = $real>,
            {
                iter.fold($real(0.0), add)
            }
        }

        impl<'item> Sum<&'item $real> for $real {
            fn sum<I>(iter: I) -> Self
            where
                I: Iterator<Item = &'item $real>,
            {
                iter.fold($real(0.0), add)
            }
        }

        real_from_unsigned_ints_impl! {
            $real{$float}:
            NonZeroU8{u8}, NonZeroU16{u16}, NonZeroU32{u32}, NonZeroUsize{usize}, NonZeroU64{u64}
        }
        real_from_signed_ints_impl! {
            $real{$float}:
            NonZeroI8{i8}, NonZeroI16{i16}, NonZeroI32{i32}, NonZeroIsize{isize}, NonZeroI64{i64}
        }

        real_int_operator_impl! {
            infallible Add::add | AddAssign::add_assign for $real{$float} and
            NonZeroU8{u8}, NonZeroU16{u16}, NonZeroU32{u32}, NonZeroUsize{usize}, NonZeroU64{u64},
            NonZeroI8{i8}, NonZeroI16{i16}, NonZeroI32{i32}, NonZeroIsize{isize}, NonZeroI64{i64}
        }

        real_int_operator_impl! {
            infallible Sub::sub | SubAssign::sub_assign for $real{$float} and
            NonZeroU8{u8}, NonZeroU16{u16}, NonZeroU32{u32}, NonZeroUsize{usize}, NonZeroU64{u64},
            NonZeroI8{i8}, NonZeroI16{i16}, NonZeroI32{i32}, NonZeroIsize{isize}, NonZeroI64{i64}
        }

        real_int_operator_impl! {
            fallible Mul::mul | MulAssign::mul_assign for $real{$float} and
            NonZeroU8{u8}, NonZeroU16{u16}, NonZeroU32{u32}, NonZeroUsize{usize}, NonZeroU64{u64},
            NonZeroI8{i8}, NonZeroI16{i16}, NonZeroI32{i32}, NonZeroIsize{isize}, NonZeroI64{i64}
        }

        real_int_operator_impl! {
            fallible Div::div | DivAssign::div_assign for $real{$float} and
            NonZeroU8{u8}, NonZeroU16{u16}, NonZeroU32{u32}, NonZeroUsize{usize}, NonZeroU64{u64},
            NonZeroI8{i8}, NonZeroI16{i16}, NonZeroI32{i32}, NonZeroIsize{isize}, NonZeroI64{i64}
        }

        real_int_operator_impl! {
            fallible Rem::rem | RemAssign::rem_assign for $real{$float} and
            NonZeroU8{u8}, NonZeroU16{u16}, NonZeroU32{u32}, NonZeroUsize{usize}, NonZeroU64{u64},
            NonZeroI8{i8}, NonZeroI16{i16}, NonZeroI32{i32}, NonZeroIsize{isize}, NonZeroI64{i64}
        }

        real_float_operator_impl! {
            $real{$float}, $float:
            Add::add | AddAssign::add_assign,
            Sub::sub | SubAssign::sub_assign,
            Mul::mul | MulAssign::mul_assign,
            Div::div | DivAssign::div_assign,
            Rem::rem | RemAssign::rem_assign
        }

        real_float_operator_impl! {
            $real{$float}, $other_float:
            Add::add | AddAssign::add_assign,
            Sub::sub | SubAssign::sub_assign,
            Mul::mul | MulAssign::mul_assign,
            Div::div | DivAssign::div_assign,
            Rem::rem | RemAssign::rem_assign
        }

        real_real_operator_impl! {
            $real{$float}, $real{$float}:
            Add::add | AddAssign::add_assign,
            Sub::sub | SubAssign::sub_assign,
            Mul::mul | MulAssign::mul_assign,
            Div::div | DivAssign::div_assign,
            Rem::rem | RemAssign::rem_assign
        }

        real_real_operator_impl! {
            $real{$float}, $other_real{$other_float}:
            Add::add | AddAssign::add_assign,
            Sub::sub | SubAssign::sub_assign,
            Mul::mul | MulAssign::mul_assign,
            Div::div | DivAssign::div_assign,
            Rem::rem | RemAssign::rem_assign
        }

        impl $real {
            pub const INFINITY: $real = $real($float::INFINITY);
            pub const NEG_INFINITY: $real = $real($float::NEG_INFINITY);
            pub const ZERO: $real = $real(0.0);
            pub const ONE: $real = $real(1.0);
            pub const PI: $real = $real(std::$float::consts::PI);
            pub const TAU: $real = $real(std::$float::consts::TAU);

            pub const fn new(x: $float) -> Option<Self> {
                if x.is_nan() {
                    None
                } else {
                    Some(Self(x))
                }
            }

            /// # Safety
            /// `x` must not be NaN
            pub const unsafe fn new_unchecked(x: $float) -> Self {
                Self(x)
            }

            pub const fn new_or(default: Self, x: $float) -> Self {
                if x.is_nan() {
                    default
                } else {
                    Self(x)
                }
            }

            pub const fn get(&self) -> $float {
                self.0
            }

            pub fn rem_euclid(self, rhs: Self) -> Option<Self> {
                Self::new(self.0.rem_euclid(rhs.0))
            }

            pub fn copysign(self, sign: Self) -> Self {
                Self(self.0.copysign(*sign))
            }

            pub fn recip(self) -> Self {
                Self(self.0.recip())
            }

            pub fn exp2(self) -> Self {
                Self(self.0.exp2())
            }

            pub fn floor(self) -> Self {
                Self(self.0.floor())
            }

            pub fn ceil(self) -> Self {
                Self(self.0.ceil())
            }

            pub fn round(self) -> Self {
                Self(self.0.round())
            }

            pub const fn is_finite(&self) -> bool {
                self.0.is_finite()
            }

            pub fn abs(self) -> Self {
                Self(self.0.abs())
            }

            pub fn sin(self) -> Option<Self> {
                Self::new(self.0.sin())
            }

            /// # Safety
            /// `self` must be finite
            pub unsafe fn sin_unchecked(self) -> Self {
                Self(self.0.sin())
            }

            pub fn sin_or(self, default: Self) -> Self {
                Self::new(self.0.sin()).unwrap_or(default)
            }

            pub fn cos(self) -> Option<Self> {
                Self::new(self.0.cos())
            }

            /// # Safety
            /// `self` must be finite
            pub unsafe fn cos_unchecked(self) -> Self {
                Self(self.0.cos())
            }

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
            $crate::real::R32::new_unchecked($x as f32)
        }
    }};

    ($x:expr) => {{
        #[allow(unused_unsafe)]
        unsafe {
            $crate::real::R64::new_unchecked(($x + 0) as f64)
        }
    }};
}

#[macro_export]
macro_rules! r64 {
    ($x:literal) => {{
        #[allow(unused_unsafe)]
        unsafe {
            $crate::real::R64::new_unchecked($x as f64)
        }
    }};

    ($x:expr) => {{
        #[allow(unused_unsafe)]
        unsafe {
            $crate::real::R64::new_unchecked(($x + 0) as f64)
        }
    }};
}
