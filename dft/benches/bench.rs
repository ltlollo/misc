#![feature(globs)]

extern crate dfts;
extern crate rand;
extern crate test;
extern crate num;

use num::complex::Complex;
use std::rand::random as random;
use test::Bencher;
use dfts::*;

#[bench]
fn mesure_dif(b: &mut Bencher) {
    let mut sig = Vec::from_fn(2048, |_| Complex::new(random::<f64>(), random::<f64>()));
    b.iter(|| dif(sig.as_mut_slice()));
}
#[bench]
fn mesure_dit(b: &mut Bencher) {
    let mut sig = Vec::from_fn(2048, |_| Complex::new(random::<f64>(), random::<f64>()));
    b.iter(|| dit(sig.as_mut_slice()));
}
#[bench]
fn mesure_dif_vec(b: &mut Bencher) {
    let ref mut sig = Vec::from_fn(2048, |_| Complex::new(random::<f64>(), random::<f64>()));
    b.iter(|| dif_vec(sig));
}
#[bench]
fn mesure_dit_vec(b: &mut Bencher) {
    let ref mut sig = Vec::from_fn(2048, |_| Complex::new(random::<f64>(), random::<f64>()));
    b.iter(|| dit_vec(sig));
}
