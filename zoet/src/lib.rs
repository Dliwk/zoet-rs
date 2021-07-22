//! Adds `#[zoet]` macro to reduce boilerplate when implementing common traits.
//!
//! If you are sick of writing `impl Deref for Bar` etc. and it didn't compile because you confused
//! it with `AsRef`, had a hard-to-debug problem because you implemented `PartialOrd` and mistakenly
//! thought that deriving `Ord` would do the sane thing, and/or you would rather just implement
//! these core traits as regular functions in your `impl Bar` like lesser languages, this crate is
//! for you!
//!
//! It is superficially similar to the various derive macros such as [`derive_more`], except that
//! rather than generating traits based on the contents of a struct, it generates them based on
//! individual functions. An example works better than a textual description ever would:
//!
//! ```
//! use core::{
//!     cmp::Ordering,
//!     hash::{Hash, Hasher},
//! };
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
//!     #[zoet(AsRef, Borrow, Deref)] // generates all of those.
//!     fn as_usize(&self) -> &usize {
//!         &self.0
//!     }
//!
//!     #[zoet(Hash)] // see note below about traits with generic functions
//!     fn hash(&self, state: &mut impl Hasher) {
//!         self.0.hash(state)
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
//! # Supported traits
//!
//! Transformations for most traits in the standard library are provided. Omitted are those which
//! are just marker traits (there's no code to generate), those which require multiple functions,
//! and some which don't quite seem worth it. The current list is as follows:
//!
//! * `core::borrow`: `Borrow`, `BorrowMut`.
//! * `core::clone`: `Clone`.
//! * `core::cmp`: `Ord`, `PartialEq`, `PartialOrd`.
//! * `core::convert`: `AsMut`, `AsRef`, `From`, `Into`, `TryFrom`, `TryInto`.
//! * `core::default`: `Default`.
//! * `core::fmt`: `Binary`, `Debug`, `Display`, `LowerExp`, `LowerHex`, `Octal`, `Pointer`,
//! `UpperExp`, `UpperHex`, `Write` (implements `write_str`).
//! * `core::future`: `Future`.
//! * `core::hash`: `Hash` (implements `hash`).
//! * `core::iterator`: `IntoIterator`, `Iterator` (implements `next`).
//! * `core::ops`: `Deref`, `DerefMut`, `Drop`, `Index`, `IndexMut`, plus all arithmetic and bit ops
//! and assignment variants such as `Add` and `AddAssign`.
//! * `core::str`: `FromStr`.
//!
//! The `alloc` feature (which is enabled by default) also adds these:
//!
//! * `alloc::borrow`: `ToOwned`.
//! * `alloc::string`: `ToString`.
//!
//! Most of the generated traits normally just include the trait boilerplate and forward the
//! arguments to your method. There are a few useful extra special cases:
//!
//! * `PartialOrd` can also be applied to an `Ord`-shaped function, in which case it wraps the
//! result with `Some()` to make it fit. This allows you to do `#zoet[(Ord, PartialOrd)]` to
//! implement both with the same function and avoid ordering-related bugs.
//!
//! * `Add` etc. can be applied to an `AddAssign`-shaped function, in which case it generates a
//! trivial implementation which mutates its `mut self` and returns it.
//!
//! # What it generates
//!
//! A suitable impl is emitted which proxies to your function, such as this:
//!
//! ```
//! # struct Length(usize);
//! # impl Length { pub fn new() -> Self { Self(0) } }
//! impl ::core::default::Default for Length {
//!     fn default() -> Self {
//!         Length::new()
//!     }
//! }
//! ```
//!
//! # Gotchas
//!
//! ## …caused by macro limitations
//!
//! Due to limitations in macro processing, you must also add `#[zoet]` to the methods' `impl` block
//! so that the self type of its associated functions can be determined. This is obviously not
//! necessary (or possible) for free functions as they don't have a self type.
//!
//! Because macros run before type checking, they only knows the _names_ of the types, and not the
//! actual types. `zoet` prefers to be liberal and pass through types rather than attempt to parse
//! them, but needs to unpick the result type used by some traits such as `TryInto` into the success
//! and error types, or rather, the _names_ of the success and error types. As such, it expects the
//! result type to be called (or be a path ending in) `Result` or `Fallible`, and if the second
//! parameter is missing, the identifier `Error` is used. Idiomatic Rust code shouldn't have a
//! problem with this, but if you have unusual error-handling, you may trip over this.
//!
//! ## …caused by trait limitations
//!
//! Traits require named lifetimes and cannot use the elided (`'_`) or implicit lifetimes. This
//! means that if you're using references, you will eventually trip on this. The only fix is to
//! change your function to use an explicit (`'a`) lifetime. This may cause the
//! `clippy::needless_lifetimes` lint to warn "explicit lifetimes given in parameter types where
//! they could be elided (or replaced with `'_` if needed by type declaration)", but it is required
//! and you'll just have to disable that lint (possibly only on that function).
//!
//! Abstract return types, aka "impl Trait in return position" is useful when you have a complex or
//! un-nameable type that you wish to return. This mainly manifests itself with `Iterator` but also
//! turns up in some `Future`s and other places. It is thus natural to want to write code like this:
//!
//! ```ignore
//! #[zoet(IntoIterator)]
//! fn iter<'a>(&'a self) -> impl Iterator<Item = &'a T> {
//!     self.as_slice().iter()
//! }
//! ```
//!
//! Unfortunately, this will not work because _[`impl Trait` can only appear as a parameter or
//! return type of a free or inherent function. It cannot appear inside implementations of traits,
//! nor can it be the type of a let binding or appear inside a type
//! alias.](https://doc.rust-lang.org/nightly/reference/types/impl-trait.html#limitations)_`zoet`
//! copies the function return type `impl Iterator<Item = &'a T>` into the `IntoIter` type inside
//! the `IntoIterator` trait, which is not allowed. A nightly compiler will suggest you "add
//! `#![feature(min_type_alias_impl_trait)]`", and if you are happy to be limited to nightly (until
//! such time as this feature is stabilised) then that's that. Otherwise, you are going to have to
//! write a concrete type:
//!
//! ```ignore
//! #[zoet(IntoIterator)]
//!     fn iter<'a>(&'a self) -> slice::Iter<'a, T> {
//!     self.as_slice().iter()
//! }
//! ```
//!
//! Unfortunately, if you have a complex iterator pipeline, you may not be able to (easily) name the
//! type. This will involve a fairly hairy refactor and/or just giving up and using `Box<dyn
//! Iterator…>`. There is nothing `zoet` can do about this, as this is a Rust limitation which you
//! will hit even if you do not use `zoet`.
//!
//! ## …caused by `zoet`'s design
//!
//! Generic parameters on the function and/or its inherent `impl` are all just accumulated and added
//! to the trait `impl`'s generic parameters, which does the right thing for the vast majority of
//! traits. However, where a trait's function is itself generic, `zoet` isn't (yet) smart enough to
//! figure out which of the generic parameter is for the function. As a perfectly good workaround,
//! use an `impl Trait` parameter instead. So while `Hash` defines its single method as `fn hash<H:
//! Hasher>(&self, state: &mut H)`, your function needs to be something like `fn hash(&self, state:
//! &mut impl Hasher)`.
//!
//! ## …caused by _you_
//!
//! While this macro makes it easy to stamp out loads of core traits, don't go crazy but consider
//! each trait you add and whether there is a more suitable macro to do the job, or indeed whether
//! that trait should be added. The example above generates `Default` based on `new()`, but since
//! that function returns 0 which is the default value anyway, it'd be better to `#derive(Default)`
//! and implement `new()` in terms of that. Similarly, its `Add` and `AddAssign` trait
//! implementations just delegate to its field's `Add` and `AddAssign` traits, and the can be
//! completely eliminated by using [`derive_more`] and deriving `Add` and `AddAssign` on the struct.
//! If your struct doesn't satisfy `Borrow`'s invariants, you shouldn't unthinkingly do
//! `#[zoet(AsRef, Borrow, Deref)]`.
//!
//! When things don't go quite as you expect, you are also reminded that [`cargo-expand`] exists and
//! can be used to inspect the expanded text.
//!
//! [`cargo-expand`]: https://crates.io/crates/cargo-expand
//! [`derive_more`]: https://crates.io/crates/derive_more

#![forbid(unsafe_code)]
#![no_std]
#![cfg_attr(feature = "unstable-doc-cfg", feature(doc_cfg))]

#[cfg(any(feature = "alloc", doc))]
#[doc(hidden)]
pub extern crate alloc as __alloc;

pub use zoet_macro::zoet;
