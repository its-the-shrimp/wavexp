#![feature(try_blocks)]
#![feature(never_type)]
#![feature(unwrap_infallible)]
#![feature(const_slice_index)]
#![feature(const_float_classify)]
#![feature(const_trait_impl)]
#![feature(const_mut_refs)]
#![feature(array_windows)]
#![feature(extract_if)]
#![feature(associated_type_defaults)]
#![feature(const_range_bounds)]
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

fn main() {
    yew::Renderer::<global::App>::new().render();
}
