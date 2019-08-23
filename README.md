Adds `#[zoet]` macro to reduce boilerplate when implementing common traits.

If you are sick of writing `impl Deref for Bar` etc. and it didn't compile because you confused it
with `AsRef`, had a hard-to-debug problem because you implemented `PartialOrd` and mistakenly
thought that deriving `Ord` would do the sane thing, and/or you would rather just implement these
core traits as regular methods in your `impl Bar` like lesser languages, this crate is for you!

Unfortunately, it uses nightly features, so if you need to use the stable compiler, you are going
to have to wait in anticipation for the features to stablilise before you can use it.

It is superficially similar to the various derive macros such as [`derive_more`], except that
rather than generating traits based on the contents of a struct, it generates them based on
individual functions/methods. An example works better than a textual description ever would:

```
use zoet::zoet;

#[derive(Clone, Copy, Debug, PartialEq)]
struct Length(usize);
#[zoet]
impl Length {
    #[zoet(Default)]          // generates `impl Default for Length`
    pub fn new() -> Self { Self(0) }

    #[zoet(From)]             // generates `From<usize> for Length`
    fn from_usize(value: usize) -> Self { Self(value) }

    #[zoet(From)]             // generates `From<Length> for usize`
    fn to_usize(self) -> usize { self.0 }

    #[zoet(AsRef, Borrow, Deref)] // generates all of those
    fn as_usize(&self) -> &usize { &self.0 }

    #[zoet(Add, AddAssign)]   // generates `impl Add for Length` and `impl AddAssign for Length`
    fn add_assign(&mut self, rhs: Self) { self.0 += rhs.0; }

    #[zoet(Ord, PartialOrd)]  // you get the idea by now
    fn ord(&self, other: &Self) { self.0.cmp(&other.0) }
}

let mut v = Length::default();
v += Length(1);
assert_eq!(v + Length(2), Length(3));
v += Length(4);
assert_eq!(v, Length(5));
assert_eq!(Length::from(v), Length(5));
```

Due to limitations in macro processing, you must add `#[zoet]` to your struct's impl block so that
the self type of its methods can be determined. This is obviously not necessary (or possible) for
free functions as they don't have a self type.

Transformations for most traits in the standard library are provided. Omitted are those which are
just marker traits (there's no code to generate), those which require multiple functions, and some
which don't quite seem worth it. The current list is as follows:

* `core::borrow`: `Borrow`, `BorrowMut`.
* `std::borrow`: `ToOwned`.
* `core::clone`: `Clone`.
* `core::cmp`: `Ord`, `PartialEq`, `PartialOrd`.
* `core::convert`: `AsMut`, `AsRef`, `From`, `Into`, `TryFrom`, `TryInto`.
* `core::default`: `Default`.
* `core::fmt`: `Debug`, `Display`, `Write` (only the `write_str` method).
* `core::iterator`: `FromIterator`, `Iterator` (only the `next` method).
* `core::ops`: `Deref`, `DerefMut`, `Drop`, `Index`, `IndexMut`, plus all arithmetic and bit ops and
  assignment variants such as `Add` and `AddAssign`.
* `core::str`: `FromStr`.
* `std::string`: `ToString`.

These traits normally just include the trait boilerplate and forward the arguments to your method,
however there are a couple of special cases which reduce boilerplate further:

* `PartialOrd` can also be applied to an `Ord`-shaped function, in which case it wraps
  the result with `Some()` to make it fit. This allows you to do `#zoet[(Ord, PartialOrd)]` to
  implement both with the same function and avoid order-related bugs.
* `Add` etc. can be applied to an `AddAssign`-shaped function, in which case it generates a trivial
  implementation which mutates its `mut self` and returns it.

However, while this macro makes it easy to stamp out loads of core traits, don't go crazy but
consider each trait you add and whether there is a more suitable macro to do the job. The example
above generates `Default` based on `new()`, but since that function returns 0 which is the default
value anyway, it'd be better to `#derive(Default)` and implement `new()` in terms of that.
Similarly, its `Add` and `AddAssign` trait implementations just delegating to its field's `Add` and
`AddAssign` traits, and the can be completely eliminated by using [`derive_more`] and deriving
`Add` and `AddAssign` on the struct. And if your struct doesn't satisfy `Borrow`'s invariants, you
shouldn't unthinkingly do `#[zoet(AsRef, Borrow, Deref)]`.

You are also reminded that [`cargo-expand`] exists, and can be used to inspect the expanded text.

[`cargo-expand`]: https://crates.io/crates/cargo-expand
[`derive_more`]: https://crates.io/crates/derive_more

