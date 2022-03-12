#![doc = include_str!("../README.md")]
#![forbid(unsafe_code)]
#![no_std]

#[cfg(feature = "alloc")]
#[doc(hidden)]
pub extern crate alloc as __alloc;

pub use zoet_macro::zoet;
