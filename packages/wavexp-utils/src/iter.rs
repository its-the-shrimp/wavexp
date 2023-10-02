pub struct EveryNth<'a, T> {
    iter: &'a [T],
    n: usize,
    state: usize,
    off: usize
}

impl<'a, T> Iterator for EveryNth<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
		if let res @Some(_) = self.iter.get(self.state) {
            self.state += self.n;
            res
        } else {
            self.off += 1;
            if self.off == self.n {
                None
            } else {
                self.state = self.off + self.n;
                self.iter.get(self.state - self.n)
            }
        }
    }
}

pub struct EveryNthMut<'a, T> {
    iter: &'a mut [T],
    n: usize,
    state: usize,
    off: usize
}

impl<'a, T> Iterator for EveryNthMut<'a, T> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
		if let Some(res) = self.iter.get_mut(self.state) {
            self.state += self.n;
            Some(unsafe{(res as *mut T).as_mut().unwrap_unchecked()})
        } else {
            self.off += 1;
            if self.off == self.n {
                None
            } else {
                self.state = self.off + self.n;
                self.iter.get_mut(self.state - self.n)
                    .map(|x| unsafe{(x as *mut T).as_mut().unwrap_unchecked()})
            }
        }
    }
}

pub trait ToEveryNth<T> {
    fn every_nth(&self, n: usize) -> EveryNth<'_, T>;
    fn every_nth_mut(&mut self, n: usize) -> EveryNthMut<'_, T>;
}

impl<T> ToEveryNth<T> for [T] {
    fn every_nth(&self, n: usize) -> EveryNth<'_, T> {
        EveryNth {iter: self, n, state: 0, off: 0}
    }
    fn every_nth_mut(&mut self, n: usize) -> EveryNthMut<'_, T> {
        EveryNthMut {iter: self, n, state: 0, off: 0}
    }
}

#[test]
fn test_every_nth_mut() {
    let mut data = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    let transposed: Vec<u8>     = data.every_nth(3).copied().collect();
    let transposed_mut: Vec<u8> = data.every_nth_mut(3).map(|x| *x).collect();
    assert_eq!(transposed,     [0, 3, 6, 9, 1, 4, 7, 10, 2, 5, 8]);
    assert_eq!(transposed_mut, [0, 3, 6, 9, 1, 4, 7, 10, 2, 5, 8]);
}

pub struct IterIndicesMut<'data, 'ids, T> {
    data: &'data mut [T],
    /// all indices are valid, trust me bro
    ids: &'ids [usize],
    state: usize
}

impl<'data, 'ids, T> Iterator for IterIndicesMut<'data, 'ids, T> {
    type Item = &'data mut T;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(&id) = self.ids.get(self.state) {
            self.state += 1;
            unsafe {
                (self.data.get_unchecked_mut(id) as *mut T).as_mut()
            }
        } else {None}
    }
}

pub trait ToIterIndicesMut<'data, 'ids, T> {
    /// # Safety
    /// all `ids` must be valid indices into `self`
    unsafe fn iter_indices_unchecked_mut(&'data mut self, ids: &'ids [usize])
    -> IterIndicesMut<'data, 'ids, T>;
    fn iter_indices_mut(&'data mut self, ids: &'ids [usize])
    -> Option<IterIndicesMut<'data, 'ids, T>>;
}

impl<'data, 'ids, T> ToIterIndicesMut<'data, 'ids, T> for [T] {
    unsafe fn iter_indices_unchecked_mut(&'data mut self, ids: &'ids [usize])
    -> IterIndicesMut<'data, 'ids, T> {
        IterIndicesMut{data: self, ids, state: 0}
    }
    
    fn iter_indices_mut(&'data mut self, ids: &'ids [usize])
    -> Option<IterIndicesMut<'data, 'ids, T>> {
        let len = self.len();
        if ids.iter().any(|&i| i >= len) {return None}
        Some(IterIndicesMut{data: self, ids, state: 0})
    }
}
