use std::{
    cell::UnsafeCell,
    cmp::Ordering,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    rc::Rc,
};

use yew::html::ImplicitClone;

use crate::{app_error, default, AppError, Result};

/// this exists to circumvent a limiatation on static variables that Rust imposes, which prevents
/// them from containing types that don't implement `Sync`. On any other architecture this
/// limitation makes sense, but in Webassembly, which doesn't support threading, this limitation is meaningless.
pub struct WasmCell<T>(pub T);

#[cfg(target_arch = "wasm32")]
unsafe impl<T> Sync for WasmCell<T> {}

impl<T> Deref for WasmCell<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct SharedRef<'src, T> {
    value: NonNull<T>,
    count: NonNull<isize>,
    marker: PhantomData<&'src T>,
}

impl<'src, T> Deref for SharedRef<'src, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { self.value.as_ref() }
    }
}

impl<'src, T> Clone for SharedRef<'src, T> {
    fn clone(&self) -> Self {
        let &Self {
            value,
            mut count,
            marker,
        } = self;
        unsafe { *count.as_mut() += 1 }
        Self {
            value,
            count,
            marker,
        }
    }
}

impl<'src, T> Drop for SharedRef<'src, T> {
    fn drop(&mut self) {
        unsafe { *self.count.as_mut() -= 1 }
    }
}

impl<'src, T> From<SharedAwareRef<'src, T>> for SharedRef<'src, T> {
    fn from(SharedAwareRef { value, outer }: SharedAwareRef<'src, T>) -> Self {
        Self {
            value,
            count: unsafe { NonNull::new_unchecked(&raw mut (*outer.0.get()).1) },
            marker: default(),
        }
    }
}

impl<'src, T> From<SharedRefMut<'src, T>> for SharedRef<'src, T> {
    fn from(
        SharedRefMut {
            value, mut count, ..
        }: SharedRefMut<'src, T>,
    ) -> Self {
        unsafe { *count.as_mut() -= 2 }
        Self {
            value,
            count,
            marker: default(),
        }
    }
}

pub struct SharedRefMut<'src, T> {
    value: NonNull<T>,
    count: NonNull<isize>,
    marker: PhantomData<&'src mut T>,
}

impl<'src, T> Deref for SharedRefMut<'src, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { self.value.as_ref() }
    }
}

impl<'src, T> DerefMut for SharedRefMut<'src, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.value.as_mut() }
    }
}

impl<'src, T> Drop for SharedRefMut<'src, T> {
    fn drop(&mut self) {
        unsafe { *self.count.as_mut() += 1 }
    }
}

impl<'src, T> From<SharedAwareRefMut<'src, T>> for SharedRefMut<'src, T> {
    fn from(
        SharedAwareRefMut {
            value,
            outer,
            marker,
        }: SharedAwareRefMut<'src, T>,
    ) -> Self {
        Self {
            value,
            count: unsafe { NonNull::new_unchecked(&raw mut (*outer.0.get()).1) },
            marker,
        }
    }
}

impl<'src, T> TryFrom<SharedRef<'src, T>> for SharedRefMut<'src, T> {
    type Error = AppError;
    fn try_from(
        SharedRef {
            value, mut count, ..
        }: SharedRef<'src, T>,
    ) -> Result<Self, Self::Error> {
        unsafe {
            if *count.as_ref() != 1 {
                return Result::Err(app_error!("borrowed more than once"));
            }
            *count.as_mut() = -1;
            Result::Ok(Self {
                value,
                count,
                marker: default(),
            })
        }
    }
}

impl<'src, T> SharedRefMut<'src, T> {
    pub fn get_sub_ref<U>(self, f: impl FnOnce(&T) -> &Shared<U>) -> Result<SharedRef<'src, U>> {
        // Safety: the contained shared reference will get locked if the function succeeds,
        // preventing double mutation
        f(unsafe { self.value.as_ref() }).get()
    }

    pub fn get_sub_ref_mut<U>(
        &self,
        f: impl FnOnce(&T) -> &Shared<U>,
    ) -> Result<SharedRefMut<'src, U>> {
        // Safety: the contained shared reference will get locked if the function succeeds,
        // preventing double mutation
        f(unsafe { self.value.as_ref() }).get_mut()
    }
}

pub struct SharedAwareRef<'src, T> {
    value: NonNull<T>,
    outer: &'src Shared<T>,
}

impl<'src, T> Deref for SharedAwareRef<'src, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { self.value.as_ref() }
    }
}

impl<'src, T> Clone for SharedAwareRef<'src, T> {
    fn clone(&self) -> Self {
        let &Self { value, outer } = self;
        unsafe { (*outer.0.get()).1 += 1 }
        Self { value, outer }
    }
}

impl<'src, T> Drop for SharedAwareRef<'src, T> {
    fn drop(&mut self) {
        unsafe { (*self.outer.0.get()).1 -= 1 }
    }
}

impl<'src, T> From<SharedAwareRefMut<'src, T>> for SharedAwareRef<'src, T> {
    fn from(SharedAwareRefMut { value, outer, .. }: SharedAwareRefMut<'src, T>) -> Self {
        unsafe { (*outer.0.get()).1 -= 2 }
        Self { value, outer }
    }
}

impl<'src, T> SharedAwareRef<'src, T> {
    pub fn outer(&self) -> Shared<T> {
        self.outer.clone()
    }
}

pub struct SharedAwareRefMut<'src, T> {
    value: NonNull<T>,
    outer: &'src Shared<T>,
    marker: PhantomData<&'src mut T>,
}

impl<'src, T> Deref for SharedAwareRefMut<'src, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { self.value.as_ref() }
    }
}

impl<'src, T> DerefMut for SharedAwareRefMut<'src, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.value.as_mut() }
    }
}

impl<'src, T> Drop for SharedAwareRefMut<'src, T> {
    fn drop(&mut self) {
        unsafe { (*self.outer.0.get()).1 += 1 }
    }
}

impl<'src, T> TryFrom<SharedAwareRef<'src, T>> for SharedAwareRefMut<'src, T> {
    type Error = AppError;
    fn try_from(
        SharedAwareRef { value, outer }: SharedAwareRef<'src, T>,
    ) -> Result<Self, Self::Error> {
        unsafe {
            let mut count = NonNull::new_unchecked(&raw mut (*outer.0.get()).1);
            if *count.as_ref() != 1 {
                return Result::Err(app_error!("borrowed more than once"));
            }
            *count.as_mut() = -1;
            Result::Ok(Self {
                value,
                outer,
                marker: default(),
            })
        }
    }
}

impl<'src, T> SharedAwareRefMut<'src, T> {
    pub fn outer(&self) -> Shared<T> {
        self.outer.clone()
    }

    pub fn get_sub_ref<U>(&self, f: impl FnOnce(&T) -> &Shared<U>) -> Result<SharedRef<'src, U>> {
        // Safety: the contained shared reference will get locked if the function succeeds,
        // preventing double mutation
        f(unsafe { self.value.as_ref() }).get()
    }

    pub fn get_sub_ref_mut<U>(
        &self,
        f: impl FnOnce(&T) -> &Shared<U>,
    ) -> Result<SharedRefMut<'src, U>> {
        // Safety: the contained shared reference will get locked if the function succeeds,
        // preventing double mutation
        f(unsafe { self.value.as_ref() }).get_mut()
    }
}

#[derive(Debug)]
pub struct Shared<T>(Rc<UnsafeCell<(T, isize)>>);

impl<T> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

impl<U, T: PartialEq<U>> PartialEq<Shared<U>> for Shared<T> {
    fn eq(&self, other: &Shared<U>) -> bool {
        let Ok(a) = self.get() else { return false };
        let Ok(b) = other.get() else { return false };
        *a == *b
    }
}

impl<U, T: PartialOrd<U>> PartialOrd<Shared<U>> for Shared<T> {
    fn partial_cmp(&self, other: &Shared<U>) -> Option<Ordering> {
        let Ok(a) = self.get() else { return None };
        let Ok(b) = other.get() else { return None };
        a.partial_cmp(&b)
    }
}

impl<T> ImplicitClone for Shared<T> {}

impl<T: Default> Default for Shared<T> {
    fn default() -> Self {
        Self(Rc::new(default()))
    }
}

impl<T> From<T> for Shared<T> {
    fn from(value: T) -> Self {
        Self(Rc::new(UnsafeCell::new((value, 0))))
    }
}

impl<T> Shared<T> {
    pub fn get(&self) -> Result<SharedRef<'_, T>> {
        let (value, count) = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        if *count < 0 {
            return Err(app_error!("already mutably borrowed"));
        }
        *count += 1;
        Ok(SharedRef {
            value: value.into(),
            count: count.into(),
            marker: default(),
        })
    }

    pub fn get_mut(&self) -> Result<SharedRefMut<'_, T>> {
        let (value, count) = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        match count.cmp(&&mut 0) {
            Ordering::Less => Err(app_error!("already mutably borrowed")),
            Ordering::Equal => {
                *count = -1;
                Ok(SharedRefMut {
                    value: value.into(),
                    count: count.into(),
                    marker: default(),
                })
            }
            Ordering::Greater => Err(app_error!("already borrowed")),
        }
    }

    pub fn get_aware(&self) -> Result<SharedAwareRef<'_, T>> {
        let (value, count) = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        if *count < 0 {
            return Err(app_error!("already mutably borrowed"));
        }
        *count += 1;
        Ok(SharedAwareRef {
            value: value.into(),
            outer: self,
        })
    }

    pub fn get_aware_mut(&self) -> Result<SharedAwareRefMut<'_, T>> {
        let (value, count) = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        match count.cmp(&&mut 0) {
            Ordering::Less => Err(app_error!("already mutably borrowed")),
            Ordering::Equal => {
                *count = -1;
                Ok(SharedAwareRefMut {
                    value: value.into(),
                    outer: self,
                    marker: default(),
                })
            }
            Ordering::Greater => Err(app_error!("already borrowed")),
        }
    }
}
