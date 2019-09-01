# CHANGELOG

## 2019-09-01: Release 0.1.3

Refactored to use `syn` 0.1.

Removed dependency on unstable features, so this macro can now be used in stable code. This also
means that `phf` is no longer used, which should improve compile times a little (but it's still
lost in the noise compared to `syn`.)

## 2019-08-26: Release 0.1.2

The example usage in the documentation didn't actually compile. This has been fixed.

The result-type parser now also accepts `Fallible`, as provided by the `failure` crate.

## 2019-08-23: Release 0.1.1

Expanded documentation somewhat.

The 0.1.0 release could generate implementations of `Ord`, `PartialEq` and `PartialOrd`, but
these were not documented. They now are.

`PartialOrd` now checks whether the function returns an `Option`, and if not, wraps the result
in `Some`. This allows one to apply `#zoet(Ord, PartialOrd)` to a single function to implement
both traits.

## 2019-07-19: Release 0.1.0

Initial release.
