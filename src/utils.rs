use wasm_bindgen::{JsCast, UnwrapThrowExt};

pub trait Check: Sized {
	#[inline]
	fn check<F>(self, f: F) -> Result<Self, Self> where F: FnOnce(&Self) -> bool {
		if f(&self) {Ok(self)} else {Err(self)}
	}
	#[inline]
	fn is_in<R>(self, range: R) -> Result<Self, Self>
	where Self: PartialOrd, R: std::ops::RangeBounds<Self> {
		if range.contains(&self) {Ok(self)} else {Err(self)}
	}
	#[inline]
	fn not_in<R>(self, range: R) -> Result<Self, Self>
	where Self: PartialOrd, R: std::ops::RangeBounds<Self> {
		if !range.contains(&self) {Ok(self)} else {Err(self)}
	}
}
impl<T> Check for T {}

pub trait Tee: Sized {
	#[inline]
	fn tee<F>(self, f: F) -> Self where F: FnOnce(&Self) {
		f(&self); self
	}
}
impl<T> Tee for T {}

pub trait Pipe: Sized {
	#[inline]
	fn pipe<O, F>(self, f: F) -> O where F: FnOnce(Self) -> O { f(self) }
}
impl<T> Pipe for T {}

pub trait Drop: Sized {
	#[inline] fn drop(self) {}
}
impl<T> Drop for T {}

pub trait Choose: Sized + Copy + Into<bool> {
	#[inline]
	fn choose<T: Sized>(self, on_true: T, on_false: T) -> T {
		if self.into() {on_true} else {on_false}
	}
}
impl Choose for bool {}

#[allow(non_camel_case_types)]
#[allow(dead_code)]
pub mod js_types {
	pub type bool = js_sys::Boolean;
	pub type number = js_sys::Number;
	pub type str = js_sys::JsString;
}

#[macro_export]
macro_rules! js_obj {
	($($t:ident $k:ident : $v:expr),*) => {
		$crate::wasm_bindgen::JsValue::from($crate::js_sys::Map::new()
			$( .set(&$crate::utils::js_types::str::from(stringify!($k)).into(),
				&*$crate::utils::js_types::$t::from($v)) )*)
	}
}
pub use js_obj;

#[macro_export]
macro_rules! js_log {
	($arg:literal) => {
		$crate::web_sys::console::log_1(&$arg.to_owned().into())
	};
	($f:literal, $($arg:expr),*) => {
		$crate::web_sys::console::log_1(&format!($f, $($arg),*).into())
	}
}
pub use js_log;

pub fn window() -> crate::web_sys::Window {
	unsafe {web_sys::window().unwrap_unchecked()}
}

pub fn document() -> crate::web_sys::Document {
	unsafe {web_sys::window().unwrap_unchecked().document().unwrap_unchecked()}
}

pub fn get_canvas_ctx(name: &str, options: &wasm_bindgen::JsValue)
-> Option<(usize, usize, web_sys::CanvasRenderingContext2d)> {
    let res = document().get_element_by_id(name)?
        .unchecked_into::<web_sys::HtmlCanvasElement>();
    Some((res.width() as usize, res.height() as usize,
        res.get_context_with_context_options("2d", options)
            .expect_throw_val("fetching the rendering context for the canvas")
            .expect_throw("fetching the rendering context for the canvas")
            .unchecked_into::<web_sys::CanvasRenderingContext2d>()))
}

pub trait ExpectThrowVal<T>: Sized {
	fn expect_throw_val(self, msg: &str) -> T;
	fn unwrap_throw_val(self) -> T {
		self.expect_throw_val("`expect_or_throw` failed")
	}
}

impl<T, E> ExpectThrowVal<T> for Result<T, E>
where E: Into<wasm_bindgen::JsValue> {
	fn expect_throw_val(self, msg: &str) -> T {
		match self {
			Ok(val)  => val,
			Err(err) => wasm_bindgen::throw_val(match err.into().dyn_into::<js_sys::Error>() {
				Ok(res) => 
					js_sys::Error::new(&*format!("panicked at `{}`: {}", msg, res.message()))
						.into(),
				Err(err) => err})}
	}
}

pub struct EveryNth<'a, T> {
    iter: &'a [T],
    n: usize,
    state: usize
}

impl<'a, T> std::iter::Iterator for EveryNth<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<<Self as std::iter::Iterator>::Item> {
		let len = self.iter.len();
		self.iter.get(self.state)
			.tee(|_| self.state = (self.state + self.n)
				.not_in(len .. (len + self.n).saturating_sub(1))
				.unwrap_or_else(|x| x - len + 1))
    }
}

pub struct EveryNthMut<'a, T> {
    iter: &'a mut [T],
    n: usize,
    state: usize
}

impl<'a, T> std::iter::Iterator for EveryNthMut<'a, T> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<&'a mut T> {
		let len = self.iter.len();
		self.iter.get_mut(self.state)
			.tee(|_| self.state = (self.state + self.n)
				.not_in(len .. (len + self.n).saturating_sub(1))
				.unwrap_or_else(|x| x - len + 1))
				// the abomination below is there solely because such a trivial task as
				// a lending iterator over mutable data can't be done any other way
				.map(|x| unsafe{(x as *mut T).as_mut().unwrap_unchecked()})
    }
}


pub trait ToEveryNth<T> {
    fn every_nth<'a>(&'a self, n: usize) -> EveryNth<'a, T>;
    fn every_nth_mut<'a>(&'a mut self, n: usize) -> EveryNthMut<'a, T>;
}

impl<T> ToEveryNth<T> for [T] {
    fn every_nth<'a>(&'a self, n: usize) -> EveryNth<'a, T> {
        EveryNth {iter: self, n, state: 0}
    }
    fn every_nth_mut<'a>(&'a mut self, n: usize) -> EveryNthMut<'a, T> {
        EveryNthMut {iter: self, n, state: 0}
    }
}
