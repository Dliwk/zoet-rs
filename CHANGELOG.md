# CHANGELOG

## 2023-05-25: Release 0.1.11

* There is extra checking on function signatures to give a useful diagnostic for `async` and `unsafe` fns.

* The generated traits now also add `#[allow(clippy::missing_trait_methods)]` since they can only
  ever implement the primary trait function and there's nothing the user of this macro can do to fix
  this short of manually implementing the trait.

* The wrapped function now also adds `#[allow(clippy::same_name_method)]` to suppress that
  diagnostic. It is reasonable to name the function after the trait method, e.g. `#[zoet(Iterator)]
  fn next(...)`

## 2022-10-29: Release 0.1.10

* `#[doc_cfg]` attributes are no longer copied from the function to the derived impls as this was a
  rust-unstable feature which has now been removed.

* Error messages improved to say _which_ trait cannot be derived: this is useful for cases where
  multiple traits are implemented on one function.

* Added `IntoFuture` trait since it is now stable.

* The generated trait functions now has `#[inline]` added since they are trivial proxy functions.

* The generated traits now has `#[allow(clippy::use_self, unused_qualifications)]` added to
  suppress pointless diagnostics.

* The wrapped function (i.e. the one you wrote) now has `#[allow(clippy::needless_lifetimes)]` added
  because "needless lifetimes" are often needed when the function is transformed into a trait.

## 2022-03-12: Release 0.1.9

* The generated trait impls now have a rustdoc header indicating which function they were generated
  from.

* The generated trait impls also add `#[allow(clippy::use_self, unused_qualifications)]` and
  `#[automatically_derived]` to stop miscellaneous lints from firing on idiomatic macro-generated
  code.

## 2021-12-11: Release 0.1.8

* Switch to 2021 edition.

* The "self replacer" refactor swapped mutability and lifetime in an expansion. The effect of this
  was that it would see e.g. `&'a mut self` and transform it into the nonsense "type" `&mut 'a
  Self`. Apparently the test suite didn't test for this, but it does now.

## 2021-09-26: Release 0.1.7

* Refactored the "self replacer" which was somewhat unreadable. This revealed a bug where it lost
  the `mut` on a `mut self` receiver. It is now fixed, although this should not have caused problems
  as the macro code generator only uses the names of the parameters, not their mutability.

* [`failure` is
  deprecated](https://github.com/rust-lang-nursery/failure#failure---a-new-error-management-story)
  and so `Fallible` has been removed as an alias for `Result`.

* The `alloc` feature is now actually checked by the macro and will result in a more useful
  diagnostic. This is a precursor to better support for feature flags in general. At this rate, it
  might even become fit for a 1.0 release.
* Handling of `Ord`, `PartialEq` and `PartialOrd` have been reworked somewhat, so you can now write
  `#[zoet(Ord, PartialEq, PartialOrd)]`

## 2020-10-05: Release 0.1.6

Added the remaining `std::fmt` formatting traits.

* Did a minor refactor so the rules can pattern-match any wrapper class, not just `Option`; this
  allows matching the signature of `Future::poll` — so `Future` is now supported.

## 2020-09-14: Release 0.1.5

* Ripped out the ad-hoc error and `compile_error!` injection and replaced it with the
  `proc_macro_error` crate. This gave the opportunity to include notes in the compiler output and
  generally improve the quality of error messages. A terrifying amount of refactoring was necessary
  to get the right data in the right place for this.

* `#[cfg]` and `#[doc_cfg]` attributes are now copied from the function to the derived impls. This
  fixes the obvious (in retrospect) problem where the trait impl would always be created, and then
  call a function which may not exist.

* The documentation previously incorrectly claimed it supported `FromIterator`; this was a typo of
  `IntoIterator`. `FromIterator` has a rather complex signature and since there is no simple and
  obvious way to write the `#[zoet(FromIterator)]`-adorned function, it's not likely to be supported
  any time soon.

## 2019-11-06: Release 0.1.4

* Added `#[allow(clippy::pedantic)]` to all generated impls. This allows you to turn on All The
  Lints and find issues in your own code without having Clippy complain about macro-generated code
  that you can't really do anything about.

* Split into separate `zoet` and `zoet-macro` crates. This is a cascading change caused by wanting
  to support `no_std` code by re-exporting names from the `alloc` crate, and thus needed the macro
  implementation in a separate crate so the main crate could have `pub` items.

* Implemented support for `Hash`.

## 2019-09-01: Release 0.1.3

* Refactored to use `syn` 0.1.

* Removed dependency on unstable features, so this macro can now be used in stable code. This also
  means that `phf` is no longer used, which should improve compile times a little (but it's still
  lost in the noise compared to `syn`.)

## 2019-08-26: Release 0.1.2

* The example usage in the documentation didn't actually compile. This has been fixed.

* The result-type parser now also accepts `Fallible`, as provided by the `failure` crate.

## 2019-08-23: Release 0.1.1

* Expanded documentation somewhat.

* The 0.1.0 release could generate implementations of `Ord`, `PartialEq` and `PartialOrd`, but these
  were not documented. They now are.

* `PartialOrd` now checks whether the function returns an `Option`, and if not, wraps the result in
  `Some`. This allows one to apply `#zoet(Ord, PartialOrd)` to a single function to implement both
  traits.

## 2019-07-19: Release 0.1.0

* Initial release.
