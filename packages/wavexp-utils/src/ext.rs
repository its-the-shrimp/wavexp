use std::{
    array::{from_fn, try_from_fn},
    iter::{successors, Sum},
    mem::ManuallyDrop,
    ops::{Add, Div, Mul, Neg, RangeBounds, Rem, Residual, Sub, Try},
    ptr,
};

use macro_rules_attribute::apply;
use wasm_bindgen::JsCast;
use web_sys::{CanvasRenderingContext2d, Document, Element, HtmlCanvasElement};

use crate::{
    bail,
    error::{report_err, Result},
    fallible,
    range::RangeBoundsExt,
    AppError, Point, Rect, RoundTo, SliceRef,
};

pub trait TransposedArray<T, const OUTER: usize, const INNER: usize> {
    fn transposed(self) -> [[T; OUTER]; INNER];
}

impl<T, const OUTER: usize, const INNER: usize> TransposedArray<T, OUTER, INNER>
    for [[T; INNER]; OUTER]
{
    // [[1, 2], [4, 5], [7, 8]] => [[1, 4, 7], [2, 5, 8]]
    fn transposed(self) -> [[T; OUTER]; INNER] {
        let original = ManuallyDrop::new(self);
        from_fn(|i| from_fn(|j| unsafe { ptr::read(&original[j][i]) }))
    }
}

pub trait ArrayExt<T, const N: usize>: Sized {
    fn zip<U, R>(self, other: [U; N], f: impl FnMut(T, U) -> R) -> [R; N];

    fn try_zip<U, R>(
        self,
        other: [U; N],
        f: impl FnMut(T, U) -> R,
    ) -> <R::Residual as Residual<[R::Output; N]>>::TryType
    where
        R: Try,
        R::Residual: Residual<[R::Output; N]>;

    fn zip_fold<U, R>(self, init: R, other: [U; N], f: impl FnMut(R, T, U) -> R) -> R;

    fn sum<R: Sum<T>>(self) -> R;

    fn fits<Range, Bound>(self, ranges: &[Range; N]) -> bool
    where
        T: PartialOrd<Bound>,
        Bound: PartialOrd<T>,
        Range: RangeBounds<Bound>;

    fn array_fit_into<Range, U>(self, ranges: [Range; N]) -> [U; N]
    where
        Range: RangeBoundsExt<U>,
        U: Ord,
        T: Into<U>,
    {
        self.zip(ranges, |i, r| r.fit(i.into()))
    }

    fn add<U>(self, other: [U; N]) -> [T; N]
    where
        T: Add<U, Output = T>,
    {
        self.zip(other, add)
    }

    fn sub<U>(self, other: [U; N]) -> [T; N]
    where
        T: Sub<U, Output = T>,
    {
        self.zip(other, sub)
    }

    fn mul<U>(self, other: [U; N]) -> [T; N]
    where
        T: Mul<U, Output = T>,
    {
        self.zip(other, mul)
    }

    fn div<U>(self, other: [U; N]) -> [T; N]
    where
        T: Div<U, Output = T>,
    {
        self.zip(other, div)
    }

    fn rem<U>(self, other: [U; N]) -> [T; N]
    where
        T: Rem<U, Output = T>,
    {
        self.zip(other, rem)
    }

    fn floor_to(self, other: [T; N]) -> [T::Output; N]
    where
        T: RoundTo,
    {
        self.zip(other, T::floor_to)
    }

    fn ceil_to(self, other: [T; N]) -> [T::Output; N]
    where
        T: RoundTo,
    {
        self.zip(other, T::ceil_to)
    }

    fn try_add<U>(self, other: [U; N]) -> Option<[T; N]>
    where
        T: Add<U, Output = Option<T>>,
    {
        self.try_zip(other, add)
    }

    fn try_sub<U>(self, other: [U; N]) -> Option<[T; N]>
    where
        T: Sub<U, Output = Option<T>>,
    {
        self.try_zip(other, sub)
    }

    fn try_mul<U>(self, other: [U; N]) -> Option<[T; N]>
    where
        T: Mul<U, Output = Option<T>>,
    {
        self.try_zip(other, mul)
    }

    fn try_div<U>(self, other: [U; N]) -> Option<[T; N]>
    where
        T: Div<U, Output = Option<T>>,
    {
        self.try_zip(other, div)
    }

    fn try_rem<U>(self, other: [U; N]) -> Option<[T; N]>
    where
        T: Rem<U, Output = Option<T>>,
    {
        self.try_zip(other, rem)
    }
}

impl<T, const N: usize> ArrayExt<T, N> for [T; N] {
    fn zip<U, R>(self, other: [U; N], mut f: impl FnMut(T, U) -> R) -> [R; N] {
        let (mut d, mut s) = (self.into_iter(), other.into_iter());
        // Safety:
        // - `d` & `s` are both iterators from `self` & `other` respectively
        // - `self` & `other` are both of length N, as is the return type
        from_fn(|_| unsafe { f(d.next().unwrap_unchecked(), s.next().unwrap_unchecked()) })
    }

    fn try_zip<U, R>(
        self,
        other: [U; N],
        mut f: impl FnMut(T, U) -> R,
    ) -> <R::Residual as Residual<[R::Output; N]>>::TryType
    where
        R: Try,
        R::Residual: Residual<[R::Output; N]>,
    {
        let (mut d, mut s) = (self.into_iter(), other.into_iter());
        try_from_fn(|_| unsafe { f(d.next().unwrap_unchecked(), s.next().unwrap_unchecked()) })
    }

    fn zip_fold<O, R>(self, init: R, other: [O; N], mut f: impl FnMut(R, T, O) -> R) -> R {
        self.into_iter()
            .zip(other)
            .fold(init, |r, (x, y)| f(r, x, y))
    }

    fn sum<R: Sum<T>>(self) -> R {
        self.into_iter().sum()
    }

    fn fits<R, Bound>(self, ranges: &[R; N]) -> bool
    where
        T: PartialOrd<Bound>,
        Bound: PartialOrd<T>,
        R: RangeBounds<Bound>,
    {
        self.iter().zip(ranges).all(|(i, r)| r.contains(i))
    }

    /*fn fit<R, O>(&self, mut values: [R; N]) -> [R; N]
    where
        T: RangeExt<O>,
        O: Clone + PartialOrd<R>,
        R: Clone + From<O>,
    {
        for (i, r) in values.iter_mut().zip(self) {
            *i = r.fit(i.clone());
        }
        values
    }*/
}

pub trait BoolExt {
    fn choose<T>(self, on_true: T, on_false: T) -> T;
    fn then_or<T>(self, default: T, f: impl FnOnce() -> T) -> T;
    fn choose_with<T>(self, default: impl FnOnce() -> T, f: impl FnOnce() -> T) -> T;
    fn then_negate<T: Neg<Output = T>>(self, val: T) -> T;
    fn then_try<T, E>(self, f: impl FnOnce() -> Result<T, E>) -> Result<Option<T>, E>;
    fn and_then<T>(self, f: impl FnOnce() -> Option<T>) -> Option<T>;
    fn flip(&mut self);
    //fn to_app_result(self) -> AppResult<()>;
}

impl BoolExt for bool {
    fn choose<T>(self, on_true: T, on_false: T) -> T {
        if self {
            on_true
        } else {
            on_false
        }
    }

    fn then_or<T>(self, default: T, f: impl FnOnce() -> T) -> T {
        if self {
            f()
        } else {
            default
        }
    }

    fn choose_with<T>(self, default: impl FnOnce() -> T, f: impl FnOnce() -> T) -> T {
        if self {
            f()
        } else {
            default()
        }
    }

    fn then_negate<T: Neg<Output = T>>(self, val: T) -> T {
        if self {
            -val
        } else {
            val
        }
    }

    fn then_try<T, E>(self, f: impl FnOnce() -> Result<T, E>) -> Result<Option<T>, E> {
        self.then(f).transpose()
    }

    fn and_then<T>(self, f: impl FnOnce() -> Option<T>) -> Option<T> {
        if self {
            f()
        } else {
            None
        }
    }

    fn flip(&mut self) {
        *self = !*self
    }

    /*fn to_app_result(self) -> AppResult<()> {
        if self {
            Ok(())
        } else {
            Err(app_error!("expected `true`, found `false`"))
        }
    }*/
}

pub trait ResultExt<T, E> {
    /*fn to_app_result(self) -> AppResult<T>
    where
        E: Display;*/
    fn map_or_default<U: Default>(self, f: impl FnOnce(T) -> U) -> U;
    fn report(self) -> Option<T>
    where
        E: Into<AppError>;
}

impl<T, E> ResultExt<T, E> for Result<T, E> {
    /*fn to_app_result(self) -> AppResult<T>
    where
        E: Display,
    {
        self.map_err(|e| AppError::new(&e.to_string()))
    }*/

    fn map_or_default<U: Default>(self, f: impl FnOnce(T) -> U) -> U {
        self.map_or_else(|_| default(), f)
    }

    fn report(self) -> Option<T>
    where
        E: Into<AppError>,
    {
        match self {
            Ok(x) => Some(x),
            Err(e) => {
                report_err(e.into().into());
                None
            }
        }
    }
}

pub trait OptionExt<T> {
    //fn to_app_result(self) -> AppResult<T>;
    fn choose<U>(&self, on_some: U, on_none: U) -> U;
    fn drop(self) -> Option<()>;
    fn get_or_try_insert<E>(&mut self, f: impl FnOnce() -> Result<T, E>) -> Result<&mut T, E>;
    fn map_or_default<U: Default>(self, f: impl FnOnce(T) -> U) -> U;
    fn try_map<U, R>(self, f: impl FnOnce(T) -> R) -> <R::Residual as Residual<Option<U>>>::TryType
    where
        R: Try<Output = U>,
        R::Residual: Residual<Option<U>>;
}

impl<T> OptionExt<T> for Option<T> {
    /*fn to_app_result(self) -> AppResult<T> {
        match self {
            Some(x) => Ok(x),
            None => Err(AppError::on_none()),
        }
    }*/

    fn choose<U>(&self, on_some: U, on_none: U) -> U {
        if self.is_some() {
            on_some
        } else {
            on_none
        }
    }

    #[allow(clippy::manual_map)]
    fn drop(self) -> Option<()> {
        match self {
            Some(_) => Some(()),
            None => None,
        }
    }

    fn get_or_try_insert<E>(&mut self, f: impl FnOnce() -> Result<T, E>) -> Result<&mut T, E> {
        if self.is_none() {
            *self = Some(f()?);
        }
        Ok(unsafe { self.as_mut().unwrap_unchecked() })
    }

    fn map_or_default<U: Default>(self, f: impl FnOnce(T) -> U) -> U {
        self.map_or_else(default, f)
    }

    fn try_map<U, R>(self, f: impl FnOnce(T) -> R) -> <R::Residual as Residual<Option<U>>>::TryType
    where
        R: Try<Output = U>,
        R::Residual: Residual<Option<U>>,
    {
        match self {
            Some(x) => Try::from_output(Some(f(x)?)),
            None => Try::from_output(None),
        }
    }
}

pub trait HtmlCanvasExt {
    fn get_2d_context(&self) -> Result<CanvasRenderingContext2d>;
    fn rect(&self) -> Rect;
    fn size(&self) -> [u32; 2];
    fn sync(&self);
}

impl HtmlCanvasExt for HtmlCanvasElement {
    #[apply(fallible!)]
    fn get_2d_context(&self) -> CanvasRenderingContext2d {
        self.get_context("2d")??.unchecked_into()
    }

    fn rect(&self) -> Rect {
        Rect(
            Point::ZERO,
            Point {
                x: self.width() as i32,
                y: self.height() as i32,
            },
        )
    }

    fn size(&self) -> [u32; 2] {
        [self.width(), self.height()]
    }

    fn sync(&self) {
        self.set_height(
            (self.client_height() as f64 / self.client_width() as f64 * self.width() as f64) as u32,
        );
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
        Rect(
            Point::ZERO,
            Point {
                x: self.client_width(),
                y: self.client_height(),
            },
        )
    }

    fn client_size(&self) -> [i32; 2] {
        [self.client_width(), self.client_height()]
    }
}

pub trait SliceExt<T> {
    fn not_empty(&self) -> bool;
    fn any(&self, f: impl FnMut(&T) -> bool) -> bool;
    fn all(&self, f: impl FnMut(&T) -> bool) -> bool;
    fn to_box(&self) -> Box<Self>
    where
        T: Clone;
    fn get_saturating(&self, id: usize) -> &T;
    fn get_saturating_mut(&mut self, id: usize) -> &mut T;
    fn get_wrapping(&self, id: usize) -> &T;
    fn get_wrapping_mut(&mut self, id: usize) -> &mut T;
    fn get_var<'this>(&'this self, ids: &[usize]) -> Option<Vec<&'this T>>;
    fn get_var_mut<'this>(&'this mut self, ids: &[usize]) -> Option<Vec<&'this mut T>>;
    fn get_aware(&self, index: usize) -> Option<SliceRef<'_, T>>;
    /// # Safety
    /// `index` must be a valid index into `self`
    unsafe fn get_unchecked_aware(&self, index: usize) -> SliceRef<'_, T>;
    fn try_split_at(&self, mid: usize) -> Option<(&[T], &[T])>;
}

impl<T> SliceExt<T> for [T] {
    fn not_empty(&self) -> bool {
        !self.is_empty()
    }

    fn any(&self, mut f: impl FnMut(&T) -> bool) -> bool {
        let mut res = false;
        for i in self {
            res |= f(i)
        }
        res
    }

    fn all(&self, mut f: impl FnMut(&T) -> bool) -> bool {
        let mut res = true;
        for i in self {
            res &= f(i)
        }
        res
    }

    fn to_box(&self) -> Box<Self>
    where
        T: Clone,
    {
        self.into()
    }

    fn get_saturating(&self, id: usize) -> &T {
        unsafe { self.get_unchecked(id.min(self.len() - 1)) }
    }

    fn get_saturating_mut(&mut self, id: usize) -> &mut T {
        unsafe { self.get_unchecked_mut(id.min(self.len() - 1)) }
    }

    fn get_wrapping(&self, id: usize) -> &T {
        unsafe { self.get_unchecked(id % self.len()) }
    }

    fn get_wrapping_mut(&mut self, id: usize) -> &mut T {
        unsafe { self.get_unchecked_mut(id % self.len()) }
    }

    fn get_var<'this>(&'this self, ids: &[usize]) -> Option<Vec<&'this T>> {
        let len = self.len();
        for (id, rest) in successors(ids.split_first(), |x| x.1.split_first()) {
            if *id >= len || rest.contains(id) {
                return None;
            }
        }
        // at this point, `ids` is guaranteed to contain unique valid indices into `self`
        let base = self.as_ptr();
        Some(ids.iter().map(|x| unsafe { &*base.add(*x) }).collect())
    }

    fn get_var_mut<'this>(&'this mut self, ids: &[usize]) -> Option<Vec<&'this mut T>> {
        let len = self.len();
        for (id, rest) in successors(ids.split_first(), |x| x.1.split_first()) {
            if *id >= len || rest.contains(id) {
                return None;
            }
        }
        // at this point, `ids` is guaranteed to contain unique valid indices into `self`
        let base = self.as_mut_ptr();
        Some(ids.iter().map(|x| unsafe { &mut *base.add(*x) }).collect())
    }

    fn get_aware(&self, index: usize) -> Option<SliceRef<'_, T>> {
        SliceRef::new(self, index)
    }

    unsafe fn get_unchecked_aware(&self, index: usize) -> SliceRef<'_, T> {
        SliceRef::raw(self.get_unchecked(index), index)
    }

    fn try_split_at(&self, mid: usize) -> Option<(&[T], &[T])> {
        if mid > self.len() {
            return None;
        }
        // SAFETY: `[ptr; mid]` and `[mid; len]` are inside `self`, which
        // fulfills the requirements of `split_at_unchecked`.
        Some(unsafe { self.split_at_unchecked(mid) })
    }
}

#[test]
fn slice_get_var() {
    let x = [1, 2, 4, 8, 16, 32, 64];
    assert_eq!(x.get_var(&[1, 3, 6]), Some(vec![&2, &8, &64]));
    assert_eq!(x.get_var(&[1, 25]), None);
    assert_eq!(x.get_var(&[1, 4, 5, 1]), None);
}

#[test]
fn slice_get_var_mut() {
    let mut x = [1, 2, 4, 8, 16, 32, 64];
    assert_eq!(
        x.get_var_mut(&[1, 3, 6]),
        Some(vec![&mut 2, &mut 8, &mut 64])
    );
    assert_eq!(x.get_var_mut(&[1, 25]), None);
    assert_eq!(x.get_var_mut(&[1, 4, 5, 1]), None);
}

pub trait VecExt<T> {
    fn try_remove(&mut self, index: usize) -> Result<T>;
    /// # Safety
    /// `index` must be a valid index into `self`
    unsafe fn remove_unchecked(&mut self, index: usize) -> T;
    fn try_swap_remove(&mut self, index: usize) -> Result<T>;
    fn try_insert(&mut self, index: usize, element: T) -> Result<&mut T>;
    fn push_unique(&mut self, value: T, f: impl Fn(&T, &T) -> bool) -> bool;
}

impl<T> VecExt<T> for Vec<T> {
    #[apply(fallible!)]
    fn try_remove(&mut self, index: usize) -> T {
        let len = self.len();
        if index >= len {
            // `len - index - 1` won't underflow if this passes
            bail!("removal index (is {index}) should be < len (is {len})")
        }
        unsafe {
            let ptr = self.as_mut_ptr().add(index);
            let ret = ptr::read(ptr);
            let new_len = len.unchecked_sub(1);
            ptr::copy(ptr.add(1), ptr, new_len.unchecked_sub(index));
            self.set_len(new_len);
            ret
        }
    }

    #[allow(clippy::arithmetic_side_effects)]
    unsafe fn remove_unchecked(&mut self, index: usize) -> T {
        let len = self.len();
        let ptr = self.as_mut_ptr().add(index);
        let ret = ptr::read(ptr);
        ptr::copy(ptr.add(1), ptr, len - index - 1);
        self.set_len(len - 1);
        ret
    }

    #[apply(fallible!)]
    fn try_swap_remove(&mut self, index: usize) -> T {
        let len = self.len();
        if index >= len {
            // `len > 0` is guaranteed if this passes
            bail!("removal index (is {index}) should be < len (is {len})")
        }
        unsafe {
            let base = self.as_mut_ptr();
            let new_len = len.unchecked_sub(1);
            let value = ptr::read(base.add(index));
            ptr::copy(base.add(new_len), base.add(index), 1);
            self.set_len(new_len);
            value
        }
    }

    #[apply(fallible!)]
    fn try_insert(&mut self, index: usize, element: T) -> &mut T {
        let len = self.len();
        if index > len {
            // `len - index` won't undeflow if this check passes
            bail!("insertion index (is {index}) should be <= len (is {len})")
        }
        if len == self.capacity() {
            // `len + 1` won't overflow if this falls through or passes
            self.try_reserve(1)?
        }
        unsafe {
            let p = self.as_mut_ptr().add(index);
            // Safety: `len - index` won't underflow because `index <= len`
            ptr::copy(p, p.add(1), len.unchecked_sub(index));
            ptr::write(p, element);
            // Safety: `len + 1` won't overflow because if it did, `try_reserve` above would fail
            self.set_len(len.unchecked_add(1));
            &mut *p
        }
    }

    fn push_unique(&mut self, value: T, f: impl Fn(&T, &T) -> bool) -> bool {
        if self.iter().any(|x| f(x, &value)) {
            return false;
        }
        self.push(value);
        true
    }
}

// Shortcut functions for common traits

pub fn default<T: Default>() -> T {
    T::default()
}

pub fn add<T: Add<U>, U>(x: T, y: U) -> T::Output {
    x.add(y)
}

pub fn sub<T: Sub<U>, U>(x: T, y: U) -> T::Output {
    x.sub(y)
}

pub fn mul<T: Mul<U>, U>(x: T, y: U) -> T::Output {
    x.mul(y)
}

pub fn div<T: Div<U>, U>(x: T, y: U) -> T::Output {
    x.div(y)
}

pub fn rem<T: Rem<U>, U>(x: T, y: U) -> T::Output {
    x.rem(y)
}

pub fn neg<T: Neg>(x: T) -> T::Output {
    x.neg()
}
