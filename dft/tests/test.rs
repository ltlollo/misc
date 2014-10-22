#![feature(globs)]

extern crate dfts;
extern crate num;

use num::complex::Complex;
use dfts::*;

#[test]
fn calc_dfts() {
    let (four, one, zero) = (Complex::new(4.0f64, 0.0), 
                             Complex::new(1.0f64, 0.0),
                             Complex::new(0.0f64, 0.0));
    let result = vec![four, zero, zero, zero, four, zero, zero, zero];
    let sig_orig = vec![one, zero, one, zero, one, zero, one, zero];
    let mut sig = sig_orig.clone();
    dif_slice(sig.as_mut_slice());
    assert!(sig == result, "testing dif_slice");

    sig = sig_orig.clone();
    dit_slice(sig.as_mut_slice());
    assert!(sig == result, "testing dit_slice");

    let ref mut sig = sig_orig.clone();
    dit(sig);
    assert!(*sig == result, "testing dif");

    let ref mut sig = sig_orig.clone();
    dif(sig);
    assert!(*sig == result, "testing dit");
}