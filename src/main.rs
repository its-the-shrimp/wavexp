#![feature(try_blocks)]
#![feature(never_type)]
#![feature(unwrap_infallible)]
#![feature(const_slice_index)]
#![feature(const_float_classify)]
#![feature(array_windows)]
#![feature(const_range_bounds)]
#![feature(lazy_cell)]
#![feature(variant_count)]
#![allow(clippy::unit_arg)]
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::comparison_chain)]

mod visual;
mod utils;
mod input;
mod sound;
mod global;

pub use js_sys;
pub use wasm_bindgen;
pub use yew;
pub use web_sys;
pub use wasm_bindgen_futures;

fn main() {
    yew::Renderer::<global::App>::new().render();
}
