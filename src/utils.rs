use wasm_bindgen::JsCast;

pub trait Check: Sized {
	#[inline] fn check<F>(self, f: F) -> Result<Self, Self>
    where F: FnOnce(&Self) -> bool {
		if f(&self) {Ok(self)} else {Err(self)}
	}

    #[inline] fn is_in<R>(self, range: R) -> Result<Self, Self>
	where Self: PartialOrd, R: std::ops::RangeBounds<Self> {
		if range.contains(&self) {Ok(self)} else {Err(self)}
	}

	#[inline] fn not_in<R>(self, range: R) -> Result<Self, Self>
	where Self: PartialOrd, R: std::ops::RangeBounds<Self> {
		if !range.contains(&self) {Ok(self)} else {Err(self)}
	}
}
impl<T> Check for T {}

pub trait Tee: Sized {
	#[inline] fn tee<F>(self, f: F) -> Self where F: FnOnce(&Self) {
        f(&self); self
	}

    #[inline] fn js_log<'a>(self, label: &'a str) -> Self 
    where Self: std::fmt::Debug {
        js_log!("{}{:?}", label, &self); self
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


pub fn get_canvas_ctx(name: &str, antialias: bool, alpha: bool)
-> Option<(f64, f64, web_sys::CanvasRenderingContext2d)> {
    let res = document().get_element_by_id(name)?
        .unchecked_into::<web_sys::HtmlCanvasElement>();
    Some((res.width() as f64, res.height() as f64,
        res.get_context_with_context_options("2d", &js_obj!{bool alpha: alpha, bool antialias: antialias})
            .ok()??
            .unchecked_into::<web_sys::CanvasRenderingContext2d>()))
}

pub fn sync_canvas(name: &str) {
    document().get_element_by_id(name)
        .and_then(|x| x.dyn_into::<web_sys::HtmlCanvasElement>().ok())
        .map(|x| x.set_height((x.client_height() as f64 / x.client_width() as f64 * 300.0) as u32));
}

fn to_error_with_msg(err: wasm_bindgen::JsValue, msg: &str) -> wasm_bindgen::JsValue {
    let s = format!("error while {}: {}", msg, 
        match err.dyn_into::<js_sys::Error>() {
            Ok(val) => val.message().into(),
            Err(val) => js_sys::Object::from(val).to_string()});
    js_sys::Error::new(&s).into()
}

pub trait ResultUtils<T>: Sized {
    fn expect_throw_with<F>(self, f: F) -> T
    where F: FnOnce() -> String;
    fn expect_throw(self, msg: &str) -> T;
}

pub type JsResult<T> = Result<T, wasm_bindgen::JsValue>;

impl<T, E> ResultUtils<T> for Result<T, E> {
    fn expect_throw_with<F>(self, f: F) -> T
    where F: FnOnce() -> String {
        match self {
            Ok(val) => val,
            Err(_) => wasm_bindgen::throw_str(&f())}
    }

    fn expect_throw(self, msg: &str) -> T {
        match self {
            Ok(val) => val,
            Err(_) => wasm_bindgen::throw_str(msg)}
    }
}

impl<T> ResultUtils<T> for Option<T> {
    fn expect_throw_with<F>(self, f: F) -> T
    where F: FnOnce() -> String {
        match self {
            Some(val) => val,
            None => wasm_bindgen::throw_str(&f())}
    }

    fn expect_throw(self, msg: &str) -> T {
        match self {
            Some(val) => val,
            None => wasm_bindgen::throw_str(msg)}
    }
}

pub trait JsResultUtils<T>: Sized {
    fn add_msg_to_err(self, msg: &str) -> Self;
    fn expect_throw_val_with<F>(self, f: F) -> T
    where F: FnOnce() -> String;
	fn expect_throw_val(self, msg: &str) -> T;
}

impl<T> JsResultUtils<T> for Result<T, wasm_bindgen::JsValue> {
    #[inline] fn add_msg_to_err(self, msg: &str) -> Self {
        self.map_err(|err| to_error_with_msg(err, msg))
    }

    fn expect_throw_val_with<F>(self, f: F) -> T
    where F: FnOnce() -> String {
		match self {
			Ok(val)  => val,
			Err(err) => wasm_bindgen::throw_val(to_error_with_msg(err, &f()))}
	}

    fn expect_throw_val(self, msg: &str) -> T {
        match self {
            Ok(val) => val,
            Err(err) => wasm_bindgen::throw_val(to_error_with_msg(err, msg))}
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
