#![crate_type = "lib"]
#![desc = "Some algoritms"]
#![license = "GPLv2"]
#![warn(non_camel_case_types)]

extern crate num;
extern crate core;
extern crate rand;
extern crate test;

use std::rand::random as random;
use num::complex::Complex;
use std::num::FromPrimitive;
use core::num::Float;
use test::Bencher;

#[allow(deprecated)]
pub fn dit(sig: &mut Vec<Complex<f64>>) {
    let len = sig.len();
    if len <= 1 {
        return;
    }
    let ref mut even = Vec::from_elem(len/2, Complex::new(0.0f64, 0.0));
    let ref mut odd = Vec::from_elem(len/2, Complex::new(0.0f64, 0.0));
    for i in range(0, len/2 as uint) {
        *even.get_mut(i) = *sig.get(2*i);
        *odd.get_mut(i) = *sig.get(2*i+1);
    }
    dit(even);
    dit(odd);
    for i in range(0, len/2 as uint) {
        let th: f64 = -(i as f64)*Float::two_pi()/(len as f64);
        let r: f64 = 1f64;
        *odd.get_mut(i) = *odd.get(i) * Complex::from_polar(&r, &th);
        *sig.get_mut(i) = *even.get(i) + *odd.get(i);
        *sig.get_mut(i+len/2) = *even.get(i) - *odd.get(i);
    }
}

#[allow(deprecated)]
pub fn dif(sig: &mut Vec<Complex<f64>>) {
    let len = sig.len();
    if len <= 1 {
        return;
    }
    let ref mut first = Vec::from_elem(len/2, Complex::new(0.0f64, 0.0));
    let ref mut second = Vec::from_elem(len/2, Complex::new(0.0f64, 0.0));
    for i in range(0, len/2 as uint) {
        *first.get_mut(i) = *sig.get(i);
        *second.get_mut(i) = *sig.get(i+len/2);
    }
    for i in range(0, len/2 as uint) {
        let th: f64 = -(i as f64)*Float::two_pi()/(len as f64);
        let r: f64 = -1f64;
        *first.get_mut(i) = *first.get(i) + *sig.get(i+len/2);
        *second.get_mut(i) = (*second.get(i) - *sig.get(i))*
                             Complex::from_polar(&r, &th);
    }
    dif(first);
    dif(second);
    for i in range(0, len/2 as uint) {
        *sig.get_mut(2*i) = *first.get(i);
        *sig.get_mut(2*i+1) = *second.get(i);
    }/* or
    *sig.get_mut(0) = *first.get(0);
    *sig.get_mut(1) = *second.get(0);
    let mut i = 1u;
    while i < len/2 {
        *sig.get_mut(2*i) = *first.get(i);
        *sig.get_mut(2*i+1) = *second.get(i);
        i += 1;
    }*/
}

pub fn dit_slice<T: FloatMath + FromPrimitive>(sig: &mut [Complex<T>]) {
    let len = sig.len();
    if len <= 1 {
        return;
    }
    let n: T = FromPrimitive::from_uint(len).unwrap();
    let r: T = FromPrimitive::from_uint(1).unwrap();
    let zero: T = FromPrimitive::from_uint(0).unwrap();
    let mut even_vec = Vec::from_elem(len/2, Complex::new(zero, zero));
    let mut odd_vec = Vec::from_elem(len/2, Complex::new(zero, zero));
    let even = even_vec.as_mut_slice();
    let odd = odd_vec.as_mut_slice();
    for i in range(0, len/2 as uint) {
        even[i] = sig[2*i];
        odd[i] = sig[2*i+1];
    }
    dit_slice(even);
    dit_slice(odd);
    for i in range(0, len/2 as uint) {
        let k: T = FromPrimitive::from_uint(i).unwrap();
        let th: T = -k*Float::two_pi()/n;
        odd[i] = odd[i] * Complex::from_polar(&r, &th);
        sig[i] = even[i] + odd[i];
        sig[i+len/2] = even[i] - odd[i];
    }
}

pub fn dif_slice<T: FloatMath + FromPrimitive>(sig: &mut [Complex<T>]) {
    let len = sig.len();
    if len <= 1 {
        return;
    }
    let n: T = FromPrimitive::from_uint(len).unwrap();
    let r: T = FromPrimitive::from_int(-1).unwrap();
    let mut vec = sig.to_vec();
    let (first, second) = vec.split_at_mut(len/2);
    for i in range(0, len/2 as uint) {
        let k: T = FromPrimitive::from_uint(i).unwrap();
        let th: T = -k*Float::two_pi()/n;
        first[i] = first[i] + sig[i+len/2];
        second[i] = (second[i]-sig[i])*Complex::from_polar(&r, &th);
    }
    dif_slice(first);
    dif_slice(second);
    for i in range(0, len/2 as uint) {
        sig[2*i] = first[i];
        sig[2*i+1] = second[i];
    } 
}

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

#[bench]
fn mesure_dif_slice(b: &mut Bencher) {
    let mut sig = Vec::from_fn(2048, |_| Complex::new(random::<f64>(), random::<f64>()));
    b.iter(|| dif_slice(sig.as_mut_slice()));
}
#[bench]
fn mesure_dit_slice(b: &mut Bencher) {
    let mut sig = Vec::from_fn(2048, |_| Complex::new(random::<f64>(), random::<f64>()));
    b.iter(|| dit_slice(sig.as_mut_slice()));
}
#[bench]
fn mesure_dif(b: &mut Bencher) {
    let ref mut sig = Vec::from_fn(2048, |_| Complex::new(random::<f64>(), random::<f64>()));
    b.iter(|| dif(sig));
}
#[bench]
fn mesure_dit(b: &mut Bencher) {
    let ref mut sig = Vec::from_fn(2048, |_| Complex::new(random::<f64>(), random::<f64>()));
    b.iter(|| dit(sig));
}
