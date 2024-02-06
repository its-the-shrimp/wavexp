#![feature(iter_advance_by)]
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
#![feature(iterator_try_collect)]
#![feature(inline_const)]
#![feature(array_try_map)]
#![feature(slice_first_last_chunk)]

mod app;
mod ctx;
mod editor;
mod img;
mod input;
mod popup;
mod project;
mod sequencer;
mod sound;
mod visual;

fn main() {
    yew::Renderer::<app::App>::new().render();
}
