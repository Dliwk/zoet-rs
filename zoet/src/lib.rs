//! Adds `#[zoet]` macro to reduce boilerplate when implementing common traits.
//!
//! If you are sick of writing `impl Deref for Bar` etc. and it didn't compile because you confused
//! it with `AsRef`, had a hard-to-debug problem because you implemented `PartialOrd` and
//! mistakenly thought that deriving `Ord` would do the sane thing, and/or you would rather just
//! implement these core traits as regular methods in your `impl Bar` like lesser languages, this
//! crate is for you!
//!
//! It is superficially similar to the various derive macros such as [`derive_more`], except that
//! rather than generating traits based on the contents of a struct, it generates them based on
//! individual functions/methods. An example works better than a textual description ever would:
//!
//! ```
//! use core::cmp::Ordering;
//! use zoet::zoet;
//!
//! #[derive(Clone, Copy, Debug, Eq, PartialEq)]
//! struct Length(usize);
//! #[zoet]
//! impl Length {
//!     #[zoet(Default)] // generates `impl Default for Length`
//!     pub fn new() -> Self {
//!         Self(0)
//!     }
//!
//!     #[zoet(From)] // generates `From<usize> for Length`
//!     fn from_usize(value: usize) -> Self {
//!         Self(value)
//!     }
//!
//!     #[zoet(From)] // generates `From<Length> for usize`
//!     fn to_usize(self) -> usize {
//!         self.0
//!     }
//!
//!     #[zoet(AsRef, Borrow, Deref)] // generates all of those
//!     fn as_usize(&self) -> &usize {
//!         &self.0
//!     }
//!
//!     #[zoet(Add, AddAssign)] // generates `impl Add for Length` and `impl AddAssign for Length`
//!     fn add_assign(&mut self, rhs: Self) {
//!         self.0 += rhs.0;
//!     }
//!
//!     #[zoet(Ord, PartialOrd)] // you get the idea by now
//!     fn ord(&self, other: &Self) -> Ordering {
//!         self.0.cmp(&other.0)
//!     }
//! }
//!
//! let mut v = Length::default();
//! v += Length(1);
//! assert_eq!(v + Length(2), Length(3));
//! v += Length(4);
//! assert_eq!(v, Length(5));
//! assert_eq!(Length::from(v), Length(5));
//! ```
//!
//! Due to limitations in macro processing, you must add `#[zoet]` to your struct's impl block so
//! that the self type of its methods can be determined. This is obviously not necessary (or
//! possible) for free functions as they don't have a self type.
//!
//! Transformations for most traits in the standard library are provided. Omitted are those which
//! are just marker traits (there's no code to generate), those which require multiple functions,
//! and some which don't quite seem worth it. The current list is as follows:
//!
//! * `core::borrow`: `Borrow`, `BorrowMut`.
//! * `std::borrow`/`alloc::borrow`: `ToOwned`.
//! * `core::clone`: `Clone`.
//! * `core::cmp`: `Ord`, `PartialEq`, `PartialOrd`.
//! * `core::convert`: `AsMut`, `AsRef`, `From`, `Into`, `TryFrom`, `TryInto`.
//! * `core::default`: `Default`.
//! * `core::fmt`: `Debug`, `Display`, `Write` (implements the `write_str` method).
//! * `core::iterator`: `FromIterator`, `Iterator` (implements the `next` method).
//! * `core::ops`: `Deref`, `DerefMut`, `Drop`, `Index`, `IndexMut`, plus all arithmetic and bit
//! ops and assignment variants such as `Add` and `AddAssign`.
//! * `core::str`: `FromStr`.
//! * `std::string`/`alloc::string`: `ToString`.
//!
//! These traits normally just include the trait boilerplate and forward the arguments to your
//! method, however there are a couple of special cases which reduce boilerplate further:
//!
//! * `PartialOrd` can also be applied to an `Ord`-shaped function, in which case it wraps the
//! result with `Some()` to make it fit. This allows you to do `#zoet[(Ord, PartialOrd)]` to
//! implement both with the same function and avoid order-related bugs.
//!
//! * `Add` etc. can be applied to an `AddAssign`-shaped function, in which case it generates a
//! trivial implementation which mutates its `mut self` and returns it.
//!
//! Because macros run before type checking, they only knows the _names_ of the types, and not the
//! actual types. `zoet` prefers to be liberal and pass through types rather than attempt to parse
//! them, but we need to unpick the result type used by some traits such as `TryInto` into the
//! success and error types, or rather, the _names_ of the success and error types. As such, it
//! expects the result type to be called (or be a path ending in) `Result` or `Fallible`, and if
//! the second parameter is missing, the identifier `Error` is used. Idiomatic Rust code shouldn't
//! have a problem with this, but if you have unusual error-handling, you may trip over this.
//!
//! However, while this macro makes it easy to stamp out loads of core traits, don't go crazy but
//! consider each trait you add and whether there is a more suitable macro to do the job. The
//! example above generates `Default` based on `new()`, but since that function returns 0 which is
//! the default value anyway, it'd be better to `#derive(Default)` and implement `new()` in terms
//! of that. Similarly, its `Add` and `AddAssign` trait implementations just delegating to its
//! field's `Add` and `AddAssign` traits, and the can be completely eliminated by using
//! [`derive_more`] and deriving `Add` and `AddAssign` on the struct. And if your struct doesn't
//! satisfy `Borrow`'s invariants, you shouldn't unthinkingly do `#[zoet(AsRef, Borrow, Deref)]`.
//!
//! You are also reminded that [`cargo-expand`] exists, and can be used to inspect the expanded
//! text.
//!
//! [`cargo-expand`]: https://crates.io/crates/cargo-expand
//! [`derive_more`]: https://crates.io/crates/derive_more

#![cfg_attr(feature = "clippy-insane", warn(
    //// Turn the "allow" lints listed by `rustc -W help` (as of 2020-04-16) into warn lints:
    absolute_paths_not_starting_with_crate, anonymous_parameters, box_pointers,
    deprecated_in_future, elided_lifetimes_in_paths, explicit_outlives_requirements,
    indirect_structural_match, keyword_idents, macro_use_extern_crate, meta_variable_misuse,
    missing_copy_implementations, missing_crate_level_docs, missing_debug_implementations,
    missing_doc_code_examples, missing_docs, non_ascii_idents, private_doc_tests,
    single_use_lifetimes, trivial_casts, trivial_numeric_casts, unreachable_pub, unsafe_code,
    unstable_features, unused_extern_crates, unused_import_braces, unused_lifetimes,
    unused_qualifications, unused_results, variant_size_differences,
    //// Ditto for clippy lint categories (see https://github.com/rust-lang/rust-clippy):
    clippy::all, clippy::cargo, clippy::nursery, clippy::pedantic, clippy::restriction,
), allow(
    //// turn off individual noisy/buggy lints enabled by broader categories above:
    // box_pointers,               // yeah, we allocate: get over it
    // elided_lifetimes_in_paths,  // it may be "deprecated", but adding <'_> everywhere is ugly
    // unreachable_pub,            // putting pub(crate) everywhere isn't helpful
))]
#![forbid(unsafe_code)]
#![no_std]

extern crate alloc;
/// Re-exports of [`alloc`] traits so [`zoet`](crate)-generated code does not require `extern crate
/// alloc`.
pub mod traits {
    pub use ::alloc::{borrow::ToOwned, string::ToString};
}

pub use zoet_macro::zoet;
