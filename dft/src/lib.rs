#![crate_type = "lib"]
#![desc = "Some algoritms"]
#![license = "GPLv2"]

extern crate num;
extern crate core;

use num::complex::Complex;
use std::num::FromPrimitive;
use core::num::Float;

pub fn dit_vec(sig: &mut Vec<Complex<f64>>) {
    let len = sig.len();
    if len <= 1 {
        return;
    }
    let ref mut even = Vec::from_elem(len/2, Complex::new(0.0f64, 0.0));
    let ref mut odd = Vec::from_elem(len/2, Complex::new(0.0f64, 0.0));
    {
        let sig_view: &Vec<Complex<f64>> = sig;
        for i in range(0, len/2 as uint) {
            even[i] = sig_view[2*i];
            odd[i] = sig_view[2*i+1];
        }
    }
    dit_vec(even);
    dit_vec(odd);
    for i in range(0, len/2 as uint) {
        let th: f64 = -(i as f64)*Float::two_pi()/(len as f64);
        let r: f64 = 1f64;
        let (odd_i, even_i): (Complex<f64>, Complex<f64>);
        unsafe {
            odd_i = *odd.as_ptr().offset(i as int);
            even_i = *even.as_ptr().offset(i as int);
        }
        odd[i] = odd_i * Complex::from_polar(&r, &th);
        sig[i] = even_i + odd_i;
        sig[i+len/2] = even_i - odd_i;
    }
}

pub fn dif_vec(sig: &mut Vec<Complex<f64>>) {
    let len = sig.len();
    if len <= 1 {
        return;
    }
    let ref mut first = Vec::from_elem(len/2, Complex::new(0.0f64, 0.0));
    let ref mut second = Vec::from_elem(len/2, Complex::new(0.0f64, 0.0));
    {
        let sig_view: &Vec<Complex<f64>> = sig;
        for i in range(0, len/2 as uint) {
            first[i] = sig_view[i];
            second[i] = sig_view[i+len/2];
        }
        for i in range(0, len/2 as uint) {
            let th: f64 = -(i as f64)*Float::two_pi()/(len as f64);
            let r: f64 = -1f64;
            let (first_i, second_i) : (Complex<f64>, Complex<f64>);
            unsafe {
                first_i = *first.as_ptr().offset(i as int);
                second_i = *second.as_ptr().offset(i as int);
            }
            first[i] = first_i + sig_view[i+len/2];
            second[i] = (second_i - sig_view[i])* Complex::from_polar(&r, &th);
        }
    }
    dif_vec(first);
    dif_vec(second);
    for i in range(0, len/2 as uint) {
        let (first_i, second_i) : (Complex<f64>, Complex<f64>);
        unsafe {
            first_i = *first.as_ptr().offset(i as int);
            second_i = *second.as_ptr().offset(i as int);
        }
        sig[2*i] = first_i;
        sig[2*i+1] = second_i;
    }
    /* possible opt:
    *sig.get_mut(0) = *first.get(0);
    *sig.get_mut(1) = *second.get(0);
    let mut i = 1u;
    while i < len/2 {
        *sig.get_mut(2*i) = *first.get(i);
        *sig.get_mut(2*i+1) = *second.get(i);
        i += 1;
    }*/
}

pub fn dit<T: FloatMath + FromPrimitive>(sig: &mut [Complex<T>]) {
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
    dit(even);
    dit(odd);
    for i in range(0, len/2 as uint) {
        let k: T = FromPrimitive::from_uint(i).unwrap();
        let th: T = -k*Float::two_pi()/n;
        odd[i] = odd[i] * Complex::from_polar(&r, &th);
        sig[i] = even[i] + odd[i];
        sig[i+len/2] = even[i] - odd[i];
    }
}

pub fn dif<T: FloatMath + FromPrimitive>(sig: &mut [Complex<T>]) {
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
    dif(first);
    dif(second);
    for i in range(0, len/2 as uint) {
        sig[2*i] = first[i];
        sig[2*i+1] = second[i];
    }
}
