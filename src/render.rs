use std::iter::Iterator;

use crate::utils::{Check, SliceExt};

pub struct EveryNth<'a, T> {
    iter: &'a [T],
    n: usize,
    state: usize
}

impl<'a, T> Iterator for EveryNth<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
		let len = self.iter.len();
		let res = self.iter.get(self.state);
        self.state = (self.state + self.n)
            .check_not_in(len .. (len + self.n).saturating_sub(1))
            .unwrap_or_else(|x| x - len + 1);
        res
    }
}

pub struct EveryNthMut<'a, T> {
    iter: &'a mut [T],
    n: usize,
    state: usize
}

impl<'a, T> Iterator for EveryNthMut<'a, T> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<&'a mut T> {
		let len = self.iter.len();
		let res = self.iter.get_mut(self.state)
            .map(|x| unsafe{(x as *mut T).as_mut().unwrap_unchecked()});
        // the abomination above is there solely because such a trivial task as
        // a lending iterator over mutable data can't be done any other way
        self.state = (self.state + self.n)
            .check_not_in(len .. (len + self.n).saturating_sub(1))
            .unwrap_or_else(|x| x - len + 1);
        res
    }
}

pub trait ToEveryNth<T> {
    fn every_nth(&self, n: usize) -> EveryNth<'_, T>;
    fn every_nth_mut(&mut self, n: usize) -> EveryNthMut<'_, T>;
}

impl<T> ToEveryNth<T> for [T] {
    #[inline] fn every_nth(&self, n: usize) -> EveryNth<'_, T> {
        EveryNth {iter: self, n, state: 0}
    }
    #[inline] fn every_nth_mut(&mut self, n: usize) -> EveryNthMut<'_, T> {
        EveryNthMut {iter: self, n, state: 0}
    }
}


#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Rgba {
	pub r: u8,
	pub g: u8,
	pub b: u8,
	pub a: u8
}

impl From<u32> for Rgba {
	fn from(val: u32) -> Self {
		Self {
			r: (val >> 24) as u8,
			g: ((val >> 16) & 0xFF) as u8,
			b: ((val >>  8) & 0xFF) as u8,
			a: (val & 0xFF) as u8}
	}
}

impl From<Rgba> for u32 {
	fn from(val: Rgba) -> Self {
		val.a as u32
		| (val.b as u32) << 8
		| (val.g as u32) << 16
		| (val.r as u32) << 24
	}
}

impl std::fmt::Display for Rgba {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "#{:02X}{:02X}{:02X}{:02X}", self.r, self.g, self.b, self.a)
	}
}

fn interp<const N: usize>(colours: &[Rgba; N], index: u8) -> Rgba {
	let index = (index as usize * (N - 1)) as f32 / 255.0;
	let lower = colours.get_saturating(index.floor() as usize);
	let upper = colours.get_saturating(index.ceil() as usize);
	let weight = (index / (N - 1) as f32).fract();
	let weight_recip = 1.0 - weight;
	Rgba {
		r: (lower.r as f32 * weight_recip + upper.r as f32 * weight) as u8,
		g: (lower.g as f32 * weight_recip + upper.g as f32 * weight) as u8,
		b: (lower.b as f32 * weight_recip + upper.b as f32 * weight) as u8,
		a: (lower.a as f32 * weight_recip + upper.a as f32 * weight) as u8}
}

pub struct Renderer {
	out_data: Vec<Rgba>,
	in_data: Vec<u8>,
	width: usize,
	height: usize
}

impl Renderer {
	const FG: Rgba = Rgba{r:0x00, g:0x69, b:0xE1, a:0xFF};
	const BG: Rgba = Rgba{r:0x18, g:0x18, b:0x18, a:0xFF};
	#[inline] pub const fn new() -> Self {
		Self{out_data: Vec::new(),
			in_data: Vec::new(),
			width: 0, height: 0}
	}

	pub fn set_size(&mut self, width: usize, height: usize) -> &mut Self {
		// TODO: correctly readjust the graph when shrinked in the UI
		self.width = width;
		self.height = height;
		self.out_data.resize(width * height, Self::BG);
		self.in_data.resize(width * height, 0);
		self
	}

	pub fn graph(&mut self) -> &mut Self {
		static mut GRADIENT: Vec<Rgba> = Vec::new();
		static mut INITIALISER: std::sync::Once = std::sync::Once::new();
		unsafe {INITIALISER.call_once(||
			GRADIENT = (0 ..= u8::MAX)
				.map(|i| interp(&[Self::BG, Self::FG], i))
				.collect())}
		for (&src, dst) in self.in_data.iter().zip(self.out_data.every_nth_mut(self.width)) {
            *dst = unsafe {*GRADIENT.get_unchecked(src as usize)};
        }
		self
	}

	#[inline] pub fn get_out_bytes(&self) -> &[u8] {
		unsafe {std::slice::from_raw_parts(
			self.out_data.as_ptr().cast::<u8>(),
			self.out_data.len() * 4)}
	}

	#[inline] pub fn get_in_buffer(&mut self) -> &mut [u8] {
		let len = self.in_data.len();
		self.in_data.copy_within(0 .. len - self.height, self.height);
		unsafe{self.in_data.get_unchecked_mut(..self.height)}
	}
}
