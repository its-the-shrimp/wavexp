use std::{
    collections::TryReserveError,
    convert::Infallible,
    hint::unreachable_unchecked,
    ops::{ControlFlow, FromResidual, Try},
};

use wasm_bindgen::{JsCast, JsValue};
use web_sys::{
    console::{warn_1, warn_2},
    HtmlElement,
};

use crate::{document, ext::HtmlDocumentExt};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AppError(js_sys::Error);

impl From<JsValue> for AppError {
    fn from(value: JsValue) -> Self {
        match value.dyn_into() {
            Result::Ok(x) => Self(x),
            Result::Err(x) => Self::new(&String::from(js_sys::Object::from(x).to_string())),
        }
    }
}

impl From<AppError> for js_sys::Error {
    fn from(value: AppError) -> Self {
        value.0
    }
}

macro_rules! impl_into_app_error {
    ($($t:ty),+ $(,)?) => {
        $(
            impl From<$t> for AppError {
                fn from(value: $t) -> Self {
                    Self::new(&value.to_string())
                }
            }
        )+
    };
}

impl_into_app_error! {
    hound::Error,
    TryReserveError,
}

/// `format!`-like macro to create an `AppError`
#[macro_export]
macro_rules! app_error {
    ($x:literal $(,)? $($arg:tt)*) => {
        $crate::AppError::new(&format!($x, $($arg)*))
    };
}

impl AppError {
    pub fn new(msg: &str) -> Self {
        Self(js_sys::Error::new(msg))
    }

    pub fn on_none() -> Self {
        Self::new("`Option` contained the `None` value")
    }
}

pub fn report_err(err: js_sys::Error) {
    warn_2(
        &err,
        &js_sys::Reflect::get(err.as_ref(), &"stack".into()).unwrap_or_else(|e| e),
    );
    if let Some(x) = document().element_dyn_into::<HtmlElement>("error-sign") {
        x.set_hidden(false)
    } else {
        warn_1(&JsValue::from("#error-sign element not found in the DOM"))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ResultV2<T, E> {
    Ok(T),
    Err(E),
}

pub type Result<T, E = AppError> = std::result::Result<T, E>;

impl<T, F, E: Into<F>> FromResidual<Result<Infallible, E>> for ResultV2<T, F> {
    fn from_residual(residual: Result<Infallible, E>) -> Self {
        let Result::Err(err) = residual else {
            unsafe { unreachable_unchecked() }
        };
        Self::Err(err.into())
    }
}

impl<T> FromResidual<Option<Infallible>> for ResultV2<T, AppError> {
    fn from_residual(_: Option<Infallible>) -> Self {
        Self::Err(AppError::on_none())
    }
}

impl<T, E> Try for ResultV2<T, E> {
    type Output = T;

    type Residual = Result<Infallible, E>;

    fn from_output(output: Self::Output) -> Self {
        Self::Ok(output)
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            Self::Ok(x) => ControlFlow::Continue(x),
            Self::Err(e) => ControlFlow::Break(Result::Err(e)),
        }
    }
}

#[macro_export]
macro_rules! type_or {
    () => {
        ()
    };
    ($t:ty) => {
        $t
    };
}

#[macro_export]
macro_rules! err_type {
    () => { ::wavexp_utils::error::AppError };
    (#[error = $t:ty]) => { $t };
    (#[$_:meta] $($t:tt)*) => { ::wavexp_utils::err_type!($($t)*) }
}

#[macro_export]
macro_rules! remove_err_type {
    (error = $($t:tt)*) => { doc = "" };
    ($($t:tt)*) => { $($t)* };
}

#[macro_export]
macro_rules! reconstruct_arg {
    (self) => { self };
    (&self) => { self };
    (&mut self) => { self };
    (($($t:tt),+): $_t:ty) => { ($(::wavexp_utils::reconstruct_arg!($t)),+) };
    ($name:ident: $_t:ty) => { $name }
}

#[macro_export]
macro_rules! fallible {
    (
        $(#[$meta:meta])* $v:vis fn $name:ident($($arg:tt)*) $(-> $ret:ty)? $body:block
    ) => {
        $(#[$meta])*
        $v fn $name($($arg)*) -> Result<::wavexp_utils::type_or!($($ret)?), ::wavexp_utils::error::AppError> {
            let res: ::wavexp_utils::error::ResultV2<_, _> = try $body;
            match res {
                ::wavexp_utils::error::ResultV2::Ok(x) => Result::Ok(x),
                ::wavexp_utils::error::ResultV2::Err(e) => Result::Err(e),
            }
        }
    };
    { $($t:tt)* } => {{
        let res: ::wavexp_utils::error::ResultV2<_, _> = try { $($t)* };
        match res {
            ::wavexp_utils::error::ResultV2::Ok(x) => Result::Ok(x),
            ::wavexp_utils::error::ResultV2::Err(e) => Result::Err(e),
        }
    }};
}
