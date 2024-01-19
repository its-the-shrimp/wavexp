use std::{
    array::from_fn,
    fmt::Display,
    iter::{successors, Sum},
    ops::{
        Add, AddAssign, DivAssign, MulAssign, Neg, Range, RangeBounds, RangeInclusive, RemAssign,
        Sub, SubAssign,
    },
    ptr,
};

use wasm_bindgen::JsCast;
use web_sys::{CanvasRenderingContext2d, Document, Element, HtmlCanvasElement};

use crate::{app_error, AppError, AppResult, Point, Rect, RoundTo, SliceRef};

pub trait ArrayExt<T, const N: usize>: Sized {
    fn zip<O, R>(self, other: [O; N], f: impl FnMut(T, O) -> R) -> [R; N];
    fn zip_fold<O, R>(self, init: R, other: [O; N], f: impl FnMut(R, T, O) -> R) -> R;
    fn add<'a, O>(self, other: &'a [O; N]) -> Self
    where
        T: AddAssign<&'a O>;
    fn sub<'a, O>(self, other: &'a [O; N]) -> Self
    where
        T: SubAssign<&'a O>;
    fn mul<'a, O>(self, other: &'a [O; N]) -> Self
    where
        T: MulAssign<&'a O>;
    fn div<'a, O>(self, other: &'a [O; N]) -> Self
    where
        T: DivAssign<&'a O>;
    fn rem<'a, O>(self, other: &'a [O; N]) -> Self
    where
        T: RemAssign<&'a O>;
    fn floor_to(self, other: Self) -> Self
    where
        T: RoundTo;
    fn ceil_to(self, other: Self) -> Self
    where
        T: RoundTo;
    fn sum<R>(self) -> R
    where
        R: Sum<T>;
    fn array_check_in<R, O>(self, ranges: &[R; N]) -> Option<Self>
    where
        T: PartialOrd<O>,
        O: PartialOrd<T>,
        R: RangeBounds<O>;
    fn fit<R, O>(&self, values: [R; N]) -> [R; N]
    where
        T: RangeExt<O>,
        O: Clone + PartialOrd<R>,
        R: Clone + From<O>;
}

impl<T, const N: usize> ArrayExt<T, N> for [T; N] {
    fn zip<O, R>(self, other: [O; N], mut f: impl FnMut(T, O) -> R) -> [R; N] {
        let (mut d, mut s) = (self.into_iter(), other.into_iter());
        // Safety:
        // - `d` & `s` are both iterators from `self` & `other` respectively
        // - `self` & `other` are both of length N, as is the return type
        from_fn(|_| unsafe { f(d.next().unwrap_unchecked(), s.next().unwrap_unchecked()) })
    }

    fn zip_fold<O, R>(self, init: R, other: [O; N], mut f: impl FnMut(R, T, O) -> R) -> R {
        self.into_iter()
            .zip(other)
            .fold(init, |r, (x, y)| f(r, x, y))
    }

    fn add<'a, O>(mut self, other: &'a [O; N]) -> Self
    where
        T: AddAssign<&'a O>,
    {
        for (dst, src) in self.iter_mut().zip(other.iter()) {
            *dst += src
        }
        self
    }

    fn sub<'a, O>(mut self, other: &'a [O; N]) -> Self
    where
        T: SubAssign<&'a O>,
    {
        for (dst, src) in self.iter_mut().zip(other.iter()) {
            *dst -= src
        }
        self
    }

    fn mul<'a, O>(mut self, other: &'a [O; N]) -> Self
    where
        T: MulAssign<&'a O>,
    {
        for (dst, src) in self.iter_mut().zip(other.iter()) {
            *dst *= src
        }
        self
    }

    fn div<'a, O>(mut self, other: &'a [O; N]) -> Self
    where
        T: DivAssign<&'a O>,
    {
        for (d, s) in self.iter_mut().zip(other.iter()) {
            *d /= s
        }
        self
    }

    fn rem<'a, O>(mut self, other: &'a [O; N]) -> Self
    where
        T: RemAssign<&'a O>,
    {
        for (d, s) in self.iter_mut().zip(other.iter()) {
            *d %= s
        }
        self
    }

    fn floor_to(self, other: Self) -> Self
    where
        T: RoundTo,
    {
        let mut iter = self.into_iter().zip(other).map(|(l, r)| l.floor_to(r));
        // Safety: both `self` & `other` are of size N, as is the return type
        from_fn(|_| unsafe { iter.next().unwrap_unchecked() })
    }

    fn ceil_to(self, other: Self) -> Self
    where
        T: RoundTo,
    {
        let mut iter = self.into_iter().zip(other).map(|(l, r)| l.ceil_to(r));
        // Safety: both `self` & `other` are of size N, as is the return type
        from_fn(|_| unsafe { iter.next().unwrap_unchecked() })
    }

    fn sum<R>(self) -> R
    where
        R: Sum<T>,
    {
        self.into_iter().sum()
    }

    fn array_check_in<R, O>(self, ranges: &[R; N]) -> Option<Self>
    where
        T: PartialOrd<O>,
        O: PartialOrd<T>,
        R: RangeBounds<O>,
    {
        self.iter()
            .zip(ranges)
            .all(|(i, r)| r.contains(i))
            .then_some(self)
    }

    fn fit<R, O>(&self, mut values: [R; N]) -> [R; N]
    where
        T: RangeExt<O>,
        O: Clone + PartialOrd<R>,
        R: Clone + From<O>,
    {
        for (i, r) in values.iter_mut().zip(self) {
            *i = r.fit(i.clone());
        }
        values
    }
}

pub trait BoolExt {
    fn choose<T>(self, on_true: T, on_false: T) -> T;
    fn then_or<T>(self, default: T, f: impl FnOnce() -> T) -> T;
    fn then_or_else<T>(self, default: impl FnOnce() -> T, f: impl FnOnce() -> T) -> T;
    fn then_negate<T: Neg<Output = T>>(self, val: T) -> T;
    fn then_try<T, E>(self, f: impl FnOnce() -> Result<T, E>) -> Result<Option<T>, E>;
    fn and_then<T>(self, f: impl FnOnce() -> Option<T>) -> Option<T>;
    fn flip(&mut self);
    fn to_app_result(self) -> AppResult<()>;
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

    fn then_or_else<T>(self, default: impl FnOnce() -> T, f: impl FnOnce() -> T) -> T {
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

    fn to_app_result(self) -> AppResult<()> {
        if self {
            Ok(())
        } else {
            Err(app_error!("expected `true`, found `false`"))
        }
    }
}

pub trait ResultExt<T, E> {
    fn to_app_result(self) -> AppResult<T>
    where
        E: Display;
    fn explain_err(self, msg: &str) -> AppResult<T>;
    fn explain_err_with(self, f: impl FnOnce() -> String) -> AppResult<T>;
    fn map_or_default<U: Default>(self, f: impl FnOnce(T) -> U) -> U;
}

impl<T, E> ResultExt<T, E> for Result<T, E> {
    fn to_app_result(self) -> AppResult<T>
    where
        E: Display,
    {
        self.map_err(|e| AppError::new(&e.to_string()))
    }

    fn explain_err(self, msg: &str) -> AppResult<T> {
        self.map_err(|_| AppError::new(msg))
    }

    fn explain_err_with(self, f: impl FnOnce() -> String) -> AppResult<T> {
        self.map_err(|_| AppError::new(&f()))
    }

    fn map_or_default<U: Default>(self, f: impl FnOnce(T) -> U) -> U {
        match self {
            Ok(x) => f(x),
            Err(_) => U::default(),
        }
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
        match self {
            Some(x) => f(x),
            None => U::default(),
        }
    }

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
    fn any(&self, f: impl FnMut(&T) -> bool) -> bool;
    fn all(&self, f: impl FnMut(&T) -> bool) -> bool;
    fn to_box(&self) -> Box<Self>
    where
        T: Clone;
    fn get_saturating(&self, id: usize) -> &T;
    fn get_saturating_mut(&mut self, id: usize) -> &mut T;
    fn get_wrapping(&self, id: usize) -> &T;
    fn get_wrapping_mut(&mut self, id: usize) -> &mut T;
    fn get_var<'a>(&'a self, ids: &[usize]) -> Option<Vec<&'a T>>;
    fn get_var_mut<'a>(&'a mut self, ids: &[usize]) -> Option<Vec<&'a mut T>>;
    fn get_aware(&self, index: usize) -> Option<SliceRef<'_, T>>;
    /// # Safety
    /// `index` must be a valid index into `self`
    unsafe fn get_unchecked_aware(&self, index: usize) -> SliceRef<'_, T>;
}

impl<T> SliceExt<T> for [T] {
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

    fn get_var<'a>(&'a self, ids: &[usize]) -> Option<Vec<&'a T>> {
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

    fn get_var_mut<'a>(&'a mut self, ids: &[usize]) -> Option<Vec<&'a mut T>> {
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
    fn try_remove(&mut self, index: usize) -> AppResult<T>;
    /// # Safety
    /// `index` must be a valid index into `self`
    unsafe fn remove_unchecked(&mut self, index: usize) -> T;
    fn try_swap_remove(&mut self, index: usize) -> AppResult<T>;
    fn try_insert(&mut self, index: usize, element: T) -> AppResult<&mut T>;
    fn push_unique(&mut self, value: T, f: impl Fn(&T, &T) -> bool) -> bool;
}

impl<T> VecExt<T> for Vec<T> {
    fn try_remove(&mut self, index: usize) -> AppResult<T> {
        let len = self.len();
        if index >= len {
            return Err(AppError::new(&format!(
                "removal index (is {index}) should be < len (is {len})"
            )));
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
            return Err(AppError::new(&format!(
                "removal index (is {index}) should be < len (is {len})"
            )));
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
            return Err(AppError::new(&format!(
                "insertion index (is {index}) should be <= len (is {len})"
            )));
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
        if self.iter().any(|x| f(x, &value)) {
            return false;
        }
        self.push(value);
        true
    }
}

pub trait RangeExt<T> {
    type RangeTy<R>;

    fn ordered(self) -> Self
    where
        T: Ord;
    fn overlap<O>(&self, other: &Self::RangeTy<O>) -> bool
    where
        O: PartialOrd<T>,
        T: PartialOrd<O>;
    fn loose_contain<O, I>(&self, item: I, offset: O) -> bool
    where
        O: Copy,
        I: PartialOrd<T> + Add<O, Output = I> + Sub<O, Output = I> + Copy,
        T: PartialOrd<I>;
    fn fit<R>(&self, item: R) -> R
    where
        T: Clone + Into<R> + PartialOrd<R>;
    /// if `value` is outside of `self`, extend `self` just enough for `value` to be inside it
    fn extend<R>(self, value: R) -> Self
    where
        T: PartialOrd<R> + From<R>;
    /// turns `x .. y` into `f(x) .. f(y)`
    fn map_bounds<R>(self, f: impl FnMut(T) -> R) -> Self::RangeTy<R>;
    fn to_pair(self) -> [T; 2];
}

impl<T> RangeExt<T> for Range<T> {
    type RangeTy<R> = Range<R>;

    fn ordered(self) -> Self
    where
        T: Ord,
    {
        if self.start > self.end {
            self.end..self.start
        } else {
            self
        }
    }

    fn overlap<O>(&self, other: &Self::RangeTy<O>) -> bool
    where
        O: PartialOrd<T>,
        T: PartialOrd<O>,
    {
        self.contains(&other.start) || self.contains(&other.end) || other.contains(&self.start)
    }

    fn loose_contain<O, I>(&self, item: I, offset: O) -> bool
    where
        O: Copy,
        I: PartialOrd<T> + Add<O, Output = I> + Sub<O, Output = I> + Copy,
        T: PartialOrd<I>,
    {
        self.overlap(&(item - offset..item + offset))
    }

    fn fit<R>(&self, item: R) -> R
    where
        T: Clone + Into<R> + PartialOrd<R>,
    {
        if self.end < item {
            self.end.clone().into()
        } else if self.start > item {
            self.start.clone().into()
        } else {
            item
        }
    }

    fn extend<R>(self, value: R) -> Self
    where
        T: PartialOrd<R> + From<R>,
    {
        if self.start > value {
            value.into()..self.end
        } else if self.end <= value {
            self.start..value.into()
        } else {
            self
        }
    }

    fn map_bounds<R>(self, mut f: impl FnMut(T) -> R) -> Self::RangeTy<R> {
        f(self.start)..f(self.end)
    }

    fn to_pair(self) -> [T; 2] {
        [self.start, self.end]
    }
}

impl<T> RangeExt<T> for RangeInclusive<T> {
    type RangeTy<R> = RangeInclusive<R>;

    fn ordered(self) -> Self
    where
        T: Ord,
    {
        if self.start() > self.end() {
            let (start, end) = self.into_inner();
            end..=start
        } else {
            self
        }
    }

    fn overlap<O>(&self, other: &Self::RangeTy<O>) -> bool
    where
        O: PartialOrd<T>,
        T: PartialOrd<O>,
    {
        self.contains(other.start()) || self.contains(other.end()) || other.contains(self.start())
    }

    fn loose_contain<O, I>(&self, item: I, offset: O) -> bool
    where
        O: Copy,
        I: PartialOrd<T> + Add<O, Output = I> + Sub<O, Output = I> + Copy,
        T: PartialOrd<I>,
    {
        self.overlap(&(item - offset..=item + offset))
    }

    fn fit<R>(&self, item: R) -> R
    where
        T: Clone + Into<R> + PartialOrd<R>,
    {
        if self.end() < &item {
            self.end().clone().into()
        } else if self.start() > &item {
            self.start().clone().into()
        } else {
            item
        }
    }

    fn extend<R>(self, value: R) -> Self
    where
        T: PartialOrd<R> + From<R>,
    {
        if self.start() > &value {
            value.into()..=self.into_inner().1
        } else if self.end() < &value {
            self.into_inner().0..=value.into()
        } else {
            self
        }
    }

    fn map_bounds<R>(self, mut f: impl FnMut(T) -> R) -> Self::RangeTy<R> {
        let (start, end) = self.into_inner();
        f(start)..=f(end)
    }

    fn to_pair(self) -> [T; 2] {
        self.into_inner().into()
    }
}
