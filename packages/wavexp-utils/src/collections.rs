/*use std::{cell::{UnsafeCell, Cell}, rc::Rc};

use crate::cell::WasmCell;

/// Advantages compared to Vec:
/// - Persistent indices that point to the same element no matter how it's moved around.
/// - Elements don't have to be `Sized`.
/// Disadvantages:
/// - All elements are boxed, increasing memory expenditure.
/// - The type itself is bigger, as it stores a normal `Vec` + its own ID.
/// - Indices take up an equivalent of 2 pointers, instead of 1 that a `usize` takes up.
pub struct SharedVec<T: ?Sized> {
    id: usize,
    inner: Vec<Rc<UnsafeCell<T>>>
}

static SHARED_VEC_CNT: WasmCell<Cell<usize>> = WasmCell(Cell::new(0));

/*impl<T: ?Sized + Clone> Clone for SharedVec<T> {
    fn clone(&self) -> Self {
        let id = SHARED_VEC_CNT.get();
        SHARED_VEC_CNT.set(id + 1);
        Self{id, inner: self.inner.iter().map(|x| UnsafeCell::clone(x)).collect()}
    }
}*/

impl<T: ?Sized> SharedVec<T> {
    
}*/
