use crate::{error::Result, real::R64};
use js_sys::Uint8Array;
use wasm_bindgen::JsCast;
use web_sys::{Blob, Document, HtmlAnchorElement, Url, Window};

#[allow(non_camel_case_types, dead_code)]
pub mod types {
    use js_sys::{Boolean, JsString, Number};
    use wasm_bindgen::JsValue;
    pub type bool = Boolean;
    pub type number = Number;
    pub type str = JsString;
    pub type any = JsValue;
}

#[macro_export]
macro_rules! js_array {
    ($($t:ident $v:expr),*) => {{
        let res = ::js_sys::Array::new();
        $( res.push(&$crate::js::types::$t::from($v)); )*
        ::wasm_bindgen::JsValue::from(res)
    }};
}

#[macro_export]
macro_rules! js_obj {
	($($t:ident $k:ident : $v:expr),*) => {
		::wasm_bindgen::JsValue::from(::js_sys::Map::new()
			$( .set(&$crate::js::types::str::from(stringify!($k)).into(),
				&*$crate::js::types::$t::from($v)) )*)
	}
}

#[macro_export]
macro_rules! js_function {
    (|| $body:expr) => {
        ::wasm_bindgen::closure::Closure::<dyn FnMut()>::new(move || $body)
            .into_js_value()
            .unchecked_into::<::js_sys::Function>()
    };
    (|$arg:ident $(: $t:ty)?| $body:expr) => {
        ::wasm_bindgen::closure::Closure::<dyn FnMut(_)>::new(move |$arg $(: $t)?| $body)
            .into_js_value()
            .unchecked_into::<::js_sys::Function>()
    };
    ($var:path) => {
        ::wasm_bindgen::closure::Closure::new($var)
            .into_js_value()
            .unchecked_into::<::js_sys::Function>()
    };
    ($obj:ident . $method:ident) => {
        ::wasm_bindgen::closure::Closure::<dyn FnMut(_)>::new(move |x| $obj.$method(x))
            .into_js_value()
            .unchecked_into::<::js_sys::Function>()
    }
}

#[macro_export]
macro_rules! js_log {
	($arg:literal) => {
        ::web_sys::console::log_1(&format!($arg).into())
	};
	($f:literal, $($arg:expr),*) => {
		::web_sys::console::log_1(&format!($f, $($arg),*).into())
	}
}

pub fn save_file(data: &[u8], filename: &str) -> Result {
    let data_js_inner = Uint8Array::new_with_length(data.len() as u32);
    data_js_inner.copy_from(data);
    let data_js = js_array![any data_js_inner.buffer()];
    let data_js = Blob::new_with_blob_sequence(&data_js)?;
    let temp = document().create_element("a")?.unchecked_into::<HtmlAnchorElement>();
    temp.set_href(&Url::create_object_url_with_blob(&data_js)?);
    temp.set_download(filename);
    temp.click();
    Ok(temp.remove())
}

pub fn window() -> Window {
    unsafe { web_sys::window().unwrap_unchecked() }
}

pub fn document() -> Document {
    unsafe { web_sys::window().unwrap_unchecked().document().unwrap_unchecked() }
}

/// returns precise current time in seconds.
pub fn now() -> Option<R64> {
    Some(R64::new(window().performance()?.now())? / 1000)
}
