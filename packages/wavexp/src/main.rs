#![feature(downcast_unchecked)]
#![feature(iter_advance_by)]
#![feature(arbitrary_self_types)]
#![feature(try_blocks)]
#![feature(never_type)]
#![feature(unwrap_infallible)]
#![feature(const_slice_index)]
#![feature(array_windows)]
#![feature(const_range_bounds)]
#![feature(lazy_cell)]
#![feature(variant_count)]
#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(try_find)]
#![allow(clippy::unit_arg)]
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::comparison_chain)]
#![deny(clippy::single_char_lifetime_names)]

mod app;
mod ctx;
mod editor;
mod img;
mod input;
mod popup;
mod sequencer;
mod sound;
mod visual;

pub use js_sys;
pub use wasm_bindgen;
pub use wasm_bindgen_futures;
pub use wavexp_utils;
pub use web_sys;
pub use yew;

fn main() {
    yew::Renderer::<app::App>::new().render();
}
