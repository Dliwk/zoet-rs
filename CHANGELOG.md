# CHANGELOG

## TODO: Release 0.1.7

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

## 2020-09-14: Release 0.1.5

* Ripped out the ad-hoc error and `compile_error!` injection and replaced it with the
  `proc_macro_error` crate. This gave the opportunity to include notes in the compiler output and
  generally improve the quality of error messages. A terrifying amount of refactoring was necessary
  to get the right data in the right place for this.

* `#[cfg]` and `#[doc_cfg]` attributes are now copied from the function to the derived impls. This
  fixes the obvious (in retrospect) problem where the trait impl would always be created, and then
  call a function which may not exist.

* The documentation previously incorrectly claimed it supported `FromIterator`; this was a typo for
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
