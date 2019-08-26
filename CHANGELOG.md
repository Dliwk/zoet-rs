# CHANGELOG

# 2019-08-26: Release 0.1.2

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
