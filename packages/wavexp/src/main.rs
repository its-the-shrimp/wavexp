#![feature(iter_advance_by)]
#![feature(try_blocks)]
#![feature(never_type)]
#![feature(unwrap_infallible)]
#![feature(array_windows)]
#![feature(const_range_bounds)]
#![feature(variant_count)]
#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(try_find)]
#![feature(iterator_try_collect)]
#![feature(array_try_map)]

mod app;
mod ctx;
mod editor;
mod img;
mod input;
mod persistence;
mod popup;
mod sequencer;
mod sound;
mod visual;

fn main() {
    yew::Renderer::<app::App>::new().render();
}
