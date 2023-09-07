use std::{ops::{Deref, DerefMut}, cell::UnsafeCell, ptr::NonNull, marker::PhantomData, rc::Rc, cmp::Ordering};

use yew::html::ImplicitClone;

use crate::{default, AppError, app_error, AppResult};

/// this exists to circumvent a limiatation on static variables that Rust imposes, which prevents
/// them from containing types that don't implement `Sync`. On any other architecture this
/// limitation makes sense, but in Webassembly, which doesn't support threading, this limitation is meaningless.
pub struct WasmCell<T>(pub T);

#[cfg(target_arch = "wasm32")]
unsafe impl<T> Sync for WasmCell<T> {}

impl<T> Deref for WasmCell<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {&self.0}
}

pub struct SharedRef<'a, T> {
    value:  NonNull<T>,
    count:  NonNull<isize>,
    marker: PhantomData<&'a T>
}

impl<'a, T> Deref for SharedRef<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe{self.value.as_ref()}
    }
}

impl<'a, T> Clone for SharedRef<'a, T> {
    fn clone(&self) -> Self {
        let &Self{value, mut count, marker} = self;
        unsafe { *count.as_mut() += 1 }
        Self{value, count, marker}
    }
}

impl<'a, T> Drop for SharedRef<'a, T> {
    fn drop(&mut self) {
        unsafe { *self.count.as_mut() -= 1 }
    }
}

impl<'a, T> From<SharedAwareRef<'a, T>> for SharedRef<'a, T> {
    fn from(SharedAwareRef{value, outer}: SharedAwareRef<'a, T>) -> Self {
        Self{value, count: unsafe{NonNull::new_unchecked(&raw mut (*outer.0.get()).1)},
            marker: default()}
    }
}

impl<'a, T> From<SharedRefMut<'a, T>> for SharedRef<'a, T> {
    fn from(SharedRefMut{value, mut count, ..}: SharedRefMut<'a, T>) -> Self {
        unsafe { *count.as_mut() -= 2 }
        Self{value, count, marker: default()}
    }
}

pub struct SharedRefMut<'a, T> {
    value: NonNull<T>,
    count: NonNull<isize>,
    marker: PhantomData<&'a mut T>
}

impl<'a, T> Deref for SharedRefMut<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe{self.value.as_ref()}
    }
}

impl<'a, T> DerefMut for SharedRefMut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe{self.value.as_mut()}
    }
}

impl<'a, T> Drop for SharedRefMut<'a, T> {
    fn drop(&mut self) {
        unsafe { *self.count.as_mut() += 1 }
    }
}

impl<'a, T> From<SharedAwareRefMut<'a, T>> for SharedRefMut<'a, T> {
    fn from(SharedAwareRefMut{value, outer, marker}: SharedAwareRefMut<'a, T>) -> Self {
        Self{value, count: unsafe{NonNull::new_unchecked(&raw mut (*outer.0.get()).1)}, marker}
    }
}

impl<'a, T> TryFrom<SharedRef<'a, T>> for SharedRefMut<'a, T> {
    type Error = AppError;
    fn try_from(SharedRef{value, mut count, ..}: SharedRef<'a, T>) -> Result<Self, Self::Error> {
        unsafe {
            if *count.as_ref() != 1 {return Err(app_error!("borrowed more than once"))}
            *count.as_mut() = -1;
            Ok(Self{value, count, marker: default()})
        }
    }
}

impl<'a, T> SharedRefMut<'a, T> {
    pub fn get_sub_ref<U>(self, f: impl FnOnce(&T) -> &Shared<U>) -> AppResult<SharedRef<'a, U>> {
        // Safety: the contained shared reference will get locked if the function succeeds,
        // preventing double mutation
        f(unsafe{self.value.as_ref()}).get()
    }

    pub fn get_sub_ref_mut<U>(&self, f: impl FnOnce(&T) -> &Shared<U>) -> AppResult<SharedRefMut<'a, U>> {
        // Safety: the contained shared reference will get locked if the function succeeds,
        // preventing double mutation
        f(unsafe{self.value.as_ref()}).get_mut()
    }
}

pub struct SharedAwareRef<'a, T> {
    value: NonNull<T>,
    outer: &'a Shared<T>
}

impl<'a, T> Deref for SharedAwareRef<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe{self.value.as_ref()}
    }
}

impl<'a, T> Clone for SharedAwareRef<'a, T> {
    fn clone(&self) -> Self {
        let &Self{value, outer} = self;
        unsafe { (*outer.0.get()).1 += 1 }
        Self{value, outer}
    }
}

impl<'a, T> Drop for SharedAwareRef<'a, T> {
    fn drop(&mut self) {
        unsafe { (*self.outer.0.get()).1 -= 1 }
    }
}

impl<'a, T> From<SharedAwareRefMut<'a, T>> for SharedAwareRef<'a, T> {
    fn from(SharedAwareRefMut{value, outer, ..}: SharedAwareRefMut<'a, T>) -> Self {
        unsafe { (*outer.0.get()).1 -= 2 }
        Self{value, outer}
    }
}

impl<'a, T> SharedAwareRef<'a, T> {
    pub fn outer(&self) -> Shared<T> {self.outer.clone()}
}

pub struct SharedAwareRefMut<'a, T> {
    value:  NonNull<T>,
    outer:  &'a Shared<T>,
    marker: PhantomData<&'a mut T>
}

impl<'a, T> Deref for SharedAwareRefMut<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe{self.value.as_ref()}
    }
}

impl<'a, T> DerefMut for SharedAwareRefMut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe{self.value.as_mut()}
    }
}

impl<'a, T> Drop for SharedAwareRefMut<'a, T> {
    fn drop(&mut self) {
        unsafe { (*self.outer.0.get()).1 += 1 }
    }
}

impl<'a, T> TryFrom<SharedAwareRef<'a, T>> for SharedAwareRefMut<'a, T> {
    type Error = AppError;
    fn try_from(SharedAwareRef{value, outer}: SharedAwareRef<'a, T>) -> Result<Self, Self::Error> {
        unsafe {
            let mut count = NonNull::new_unchecked(&raw mut (*outer.0.get()).1);
            if *count.as_ref() != 1 {return Err(app_error!("borrowed more than once"))}
            *count.as_mut() = -1;
            Ok(Self{value, outer, marker: default()})
        }
    }
}

impl<'a, T> SharedAwareRefMut<'a, T> {
    pub fn outer(&self) -> Shared<T> {self.outer.clone()}

    pub fn get_sub_ref<U>(&self, f: impl FnOnce(&T) -> &Shared<U>) -> AppResult<SharedRef<'a, U>> {
        // Safety: the contained shared reference will get locked if the function succeeds,
        // preventing double mutation
        f(unsafe{self.value.as_ref()}).get()
    }

    pub fn get_sub_ref_mut<U>(&self, f: impl FnOnce(&T) -> &Shared<U>) -> AppResult<SharedRefMut<'a, U>> {
        // Safety: the contained shared reference will get locked if the function succeeds,
        // preventing double mutation
        f(unsafe{self.value.as_ref()}).get_mut()
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
        let Ok(a) = self.get()  else {return false};
        let Ok(b) = other.get() else {return false};
        *a == *b
    }
}

impl<U, T: PartialOrd<U>> PartialOrd<Shared<U>> for Shared<T> {
    fn partial_cmp(&self, other: &Shared<U>) -> Option<Ordering> {
        let Ok(a) = self.get()  else {return None};
        let Ok(b) = other.get() else {return None};
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
    pub fn get(&self) -> AppResult<SharedRef<'_, T>> {
        let (value, count) = unsafe{self.0.get().as_mut().unwrap_unchecked()};
        if *count < 0 {return Err(app_error!("already mutably borrowed"))}
        *count += 1;
        Ok(SharedRef{value: value.into(), count: count.into(), marker: default()})
    }

    pub fn get_mut(&self) -> AppResult<SharedRefMut<'_, T>> {
        let (value, count) = unsafe{self.0.get().as_mut().unwrap_unchecked()};
        match count.cmp(& &mut 0) {
            Ordering::Less =>
                Err(app_error!("already mutably borrowed")),
            Ordering::Equal => {
                *count = -1;
                Ok(SharedRefMut{value: value.into(), count: count.into(), marker: default()})
            }
            Ordering::Greater =>
                Err(app_error!("already borrowed"))
        }
    }

    pub fn get_aware(&self) -> AppResult<SharedAwareRef<'_, T>> {
        let (value, count) = unsafe{self.0.get().as_mut().unwrap_unchecked()};
        if *count < 0 {return Err(app_error!("already mutably borrowed"))}
        *count += 1;
        Ok(SharedAwareRef{value: value.into(), outer: self})
    }

    pub fn get_aware_mut(&self) -> AppResult<SharedAwareRefMut<'_, T>> {
        let (value, count) = unsafe{self.0.get().as_mut().unwrap_unchecked()};
        match count.cmp(& &mut 0) {
            Ordering::Less =>
                Err(app_error!("already mutably borrowed")),
            Ordering::Equal => {
                *count = -1;
                Ok(SharedAwareRefMut{value: value.into(), outer: self, marker: default()})
            }
            Ordering::Greater =>
                Err(app_error!("already borrowed"))
        }
    }
}
