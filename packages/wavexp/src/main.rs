#![feature(try_blocks)]
#![feature(never_type)]
#![feature(unwrap_infallible)]
#![feature(const_slice_index)]
#![feature(array_windows)]
#![feature(const_range_bounds)]
#![feature(lazy_cell)]
#![feature(variant_count)]
#![feature(let_chains)]
#![feature(try_find)]
#![allow(clippy::unit_arg)]
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::comparison_chain)]

mod visual;
mod input;
mod sound;
mod global;
mod sequencer;

pub use wavexp_utils;
pub use js_sys;
pub use wasm_bindgen;
pub use yew;
pub use web_sys;
pub use wasm_bindgen_futures;

fn main() {
    yew::Renderer::<global::App>::new().render();
}
