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

use crate::MainCmd;

pub fn window() -> crate::web_sys::Window {
	unsafe {web_sys::window().unwrap_unchecked()}
}

pub fn document() -> crate::web_sys::Document {
	unsafe {web_sys::window().unwrap_unchecked().document().unwrap_unchecked()}
}


pub fn get_canvas_ctx(name: &str, antialias: bool, alpha: bool)
-> JsResult<(f64, f64, web_sys::CanvasRenderingContext2d)> {
    let res = document().get_element_by_id(name)
        .ok_or_js_error_with(|| format!("canvas #{} not found", name))?
        .unchecked_into::<web_sys::HtmlCanvasElement>();
    Ok((res.width() as f64, res.height() as f64,
        res.get_context_with_context_options("2d", &js_obj!{bool alpha: alpha, bool antialias: antialias})?
            .ok_or_js_error_with(|| format!("rendering context not found for canvas #{}", name))?
            .unchecked_into::<web_sys::CanvasRenderingContext2d>()))
}

pub fn sync_canvas(name: &str) -> JsResult<()> {
    let err: JsResult<!> = try {
        let comp = document().get_element_by_id(name)
            .ok_or_js_error("element not found")?;
        let comp = comp.dyn_into::<web_sys::HtmlCanvasElement>().ok()
            .ok_or_js_error("the element is not a <canvas>")?;
        comp.set_height((comp.client_height() as f64 / comp.client_width() as f64 * 300.0) as u32);
        return Ok(())
    };
    Err(err.explain_err_with(|| format!("syncing the dimensions of the element #{}", name)).into_err())
}

fn to_error_with_msg(err: wasm_bindgen::JsValue, msg: &str) -> wasm_bindgen::JsValue {
    let s = format!("error while {}: {}", msg, 
        match err.dyn_into::<js_sys::Error>() {
            Ok(val) => val.message().into(),
            Err(val) => js_sys::Object::from(val).to_string()});
    js_sys::Error::new(&s).into()
}

pub type JsResult<T> = Result<T, wasm_bindgen::JsValue>;

pub trait OkOrJsError<T>: Sized {
    fn ok_or_js_error_with(self, f: impl FnOnce() -> String) -> JsResult<T>;
    fn ok_or_js_error(self, msg: &str) -> JsResult<T>;
}

impl<T> OkOrJsError<T> for Option<T> {
    #[inline] fn ok_or_js_error_with(self, f: impl FnOnce() -> String) -> JsResult<T> {
        self.ok_or_else(|| js_sys::Error::new(&f()).into())
    }

    #[inline] fn ok_or_js_error(self, msg: &str) -> JsResult<T> {
        self.ok_or_else(|| js_sys::Error::new(msg).into())
    }
}

pub trait ToJsResult<T> {
    fn to_js_result(self) -> JsResult<T>;
}

impl<T, E: std::fmt::Display> ToJsResult<T> for Result<T, E> {
    fn to_js_result(self) -> JsResult<T> {
        self.map_err(|e| e.to_string().into())
    }
}

pub trait JsResultUtils<T>: Sized {
    fn explain_err_with(self, f: impl FnOnce() -> String) -> Self;
    fn explain_err(self, msg: &str) -> Self;
    fn expect_throw_with(self, f: impl FnOnce() -> String) -> T;
	fn expect_throw(self, msg: &str) -> T;
    fn report_err_with(self, f: impl FnOnce() -> String) -> Self;
	fn report_err(self, msg: &str) -> Self;
}

impl<T> JsResultUtils<T> for Result<T, wasm_bindgen::JsValue> {
    #[inline] fn explain_err_with(self, f: impl FnOnce() -> String) -> Self {
        self.map_err(|err| to_error_with_msg(err, &f()))
    }
    #[inline] fn explain_err(self, msg: &str) -> Self {
        self.map_err(|err| to_error_with_msg(err, msg))
    }

    #[inline] fn expect_throw_with(self, f: impl FnOnce() -> String) -> T {
		match self {
			Ok(val)  => val,
			Err(err) => wasm_bindgen::throw_val(to_error_with_msg(err, &f()))}
	}

    #[inline] fn expect_throw(self, msg: &str) -> T {
        match self {
            Ok(val) => val,
            Err(err) => wasm_bindgen::throw_val(to_error_with_msg(err, msg))}
    }

    #[inline] fn report_err_with(self, f: impl FnOnce() -> String) -> Self {
        self.inspect_err(|err|
            MainCmd::ReportError(to_error_with_msg(err.clone(), &f())).send())
    }

    #[inline] fn report_err(self, msg: &str) -> Self {
        self.inspect_err(|err|
            MainCmd::ReportError(to_error_with_msg(err.clone(), msg)).send())
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
    #[inline] fn every_nth<'a>(&'a self, n: usize) -> EveryNth<'a, T> {
        EveryNth {iter: self, n, state: 0}
    }
    #[inline] fn every_nth_mut<'a>(&'a mut self, n: usize) -> EveryNthMut<'a, T> {
        EveryNthMut {iter: self, n, state: 0}
    }
}

// this exists to circumvent a limiatation on static variables that Rust imposes, which prevents
// them from containing types that don't implement `Sync`. On any other architecture this
// limitation makes sense, but in Webassembly, which doesn't support threading, this limitation is meaningless.
pub struct WasmCell<T>(T);

unsafe impl<T> Sync for WasmCell<T> {}

impl<T> std::ops::Deref for WasmCell<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {&self.0}
}

impl<T> WasmCell<T> {
    pub const fn new(val: T) -> Self {Self(val)}
}

pub struct MaybeCell<T>(std::cell::RefCell<Option<T>>);

impl<T> MaybeCell<T> {
    #[inline] pub const fn new() -> Self {
        Self(std::cell::RefCell::new(None))
    }

    #[inline] pub fn get<'a>(&'a self) -> JsResult<std::cell::Ref<'a, T>> {
        std::cell::Ref::filter_map(self.0.try_borrow().to_js_result()?,
            |x| x.as_ref()).ok().ok_or_js_error("MaybeCell object not initialised")
    }

    #[inline] pub fn get_mut<'a>(&'a self) -> JsResult<std::cell::RefMut<'a, T>> {
        std::cell::RefMut::filter_map(self.0.try_borrow_mut().to_js_result()?,
            |x| x.as_mut()).ok().ok_or_js_error("MaybeCell object not initialised")
    }

    #[inline] pub fn set(&self, val: T) -> JsResult<()> {
        std::cell::RefMut::map(self.0.try_borrow_mut().to_js_result()?,
            |x| x.insert(val));
        Ok(())
    }
}
