use std::{
    collections::TryReserveError,
    convert::Infallible,
    hint::unreachable_unchecked,
    num::TryFromIntError,
    ops::{ControlFlow, FromResidual, Try},
    str::Utf8Error,
};

use wasm_bindgen::{JsCast, JsValue};
use web_sys::{
    console::{warn_1, warn_2},
    HtmlElement,
};

use crate::{ext::HtmlDocumentExt, js::document};

#[derive(Debug, Clone, PartialEq, Eq)]
// TODO: optimise by using an enum to delay conversion to a JsValue
pub struct AppError(js_sys::Error);

impl From<Infallible> for AppError {
    fn from(value: Infallible) -> Self {
        match value {}
    }
}

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
    Utf8Error,
    TryFromIntError,
}

/// `format!`-like macro to create an `AppError`
#[macro_export]
macro_rules! app_error {
    ($($arg:tt)*) => {
        ::wavexp_utils::error::AppError::new(&format!($($arg)*))
    };
}

/// a copy of `anyhow`'s `bail!` macro
#[macro_export]
macro_rules! bail {
    ($($arg:tt)*) => {
        return Err(::wavexp_utils::app_error!($($arg)*))
    };
}

#[macro_export]
macro_rules! ensure {
    ($cond:expr, $($msg:tt)*) => {
        if !$cond {
            ::wavexp_utils::bail!($($msg)*)
        }
    };
    (let $p:pat = $e:expr, $($msg:tt)*) => {
        let $p = $e else {
            ::wavexp_utils::bail!($($msg)*)
        };
    }
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
    warn_2(&err, &js_sys::Reflect::get(err.as_ref(), &"stack".into()).unwrap_or_else(|e| e));
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

pub type Result<T = (), E = AppError> = std::result::Result<T, E>;

impl<T, F, E: Into<F>> FromResidual<Result<Infallible, E>> for ResultV2<T, F> {
    fn from_residual(residual: Result<Infallible, E>) -> Self {
        let Result::Err(err) = residual else { unsafe { unreachable_unchecked() } };
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
macro_rules! fallible {
    (
        $(#[$meta:meta])* $v:vis fn $name:ident($($arg:tt)*) $(-> $ret:ty)? $body:block
    ) => {
        $(#[$meta])*
        $v fn $name($($arg)*) -> ::wavexp_utils::error::Result<::wavexp_utils::type_or!($($ret)?)> {
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
