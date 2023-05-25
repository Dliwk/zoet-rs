# Adds `#[zoet]` macro to reduce boilerplate when implementing common traits.

If you are sick of writing `impl Deref for Bar` etc., and it didn't compile because you confused it
with `AsRef`, had a hard-to-debug problem because you implemented `PartialOrd` and mistakenly
thought that deriving `Ord` would do the sane thing, and/or you would rather just implement these
core traits as regular functions in your `impl Bar` like lesser languages, this crate is for you!

`zoet` is superficially similar to the various derive macros such as [`derive_more`], except that
rather than generating traits based on the contents of a struct, it generates them based on
individual functions. An example works better than a textual description ever would:

```rust
use core::{
    cmp::Ordering,
    hash::{Hash, Hasher},
};
use zoet::zoet;

#[derive(Clone, Copy, Debug, Eq)]
struct Length(usize);
#[zoet]
impl Length {
    #[zoet(Default)] // generates `impl Default for Length`
    pub fn new() -> Self {
        Self(0)
    }

    #[zoet(From)] // generates `From<usize> for Length`
    fn from_usize(value: usize) -> Self {
        Self(value)
    }

    #[zoet(From)] // generates `From<Length> for usize`
    fn to_usize(self) -> usize {
        self.0
    }

    #[zoet(AsRef, Borrow, Deref)] // generates all of those.
    fn as_usize(&self) -> &usize {
        &self.0
    }

    #[zoet(Hash)] // see note below about traits with generic functions
    fn hash(&self, state: &mut impl Hasher) {
        self.0.hash(state)
    }

    #[zoet(Add, AddAssign)] // generates `impl Add for Length` and `impl AddAssign for Length`
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
    }

    #[zoet(Ord, PartialOrd, PartialEq)] // you get the idea by now
    fn ord(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

let mut v = Length::default();
v += Length(1);
assert_eq!(v + Length(2), Length(3));
v += Length(4);
assert_eq!(v, Length(5));
assert_eq!(Length::from(v), Length(5));
```

## Supported traits

Transformations for most traits in the standard library (`core`, `alloc`, and/or `std` crates) are
provided. The current list is as follows:

* `core::borrow`: `Borrow` and `BorrowMut`.
* `core::clone`: `Clone`.
* `core::cmp`: `Ord`, `PartialEq`, and `PartialOrd`. (`Eq` is recognised, but you'll be told to
  `#[derive(Eq)]` instead).
* `core::convert`: `AsMut`, `AsRef`, `From`, `Into`, `TryFrom`, and `TryInto`.
* `core::default`: `Default`.
* `core::fmt`: `Binary`, `Debug`, `Display`, `LowerExp`, `LowerHex`, `Octal`, `Pointer`, `UpperExp`
  `UpperHex`, and `Write` (implements `write_str`).
* `core::future`: `Future` and `IntoFuture`.
* `core::hash`: `Hash` (implements `hash`).
* `core::iter`: `IntoIterator` and `Iterator` (implements `next`).
* `core::ops`: `Deref`, `DerefMut`, `Drop`, `Index`, `IndexMut`, plus all arithmetic and bitwise
  operations, and assignment variants such as `Add` and `AddAssign`.
* `core::str`: `FromStr`.

The `alloc` feature (which is enabled by default) also adds these:

* `alloc::borrow`: `ToOwned`.
* `alloc::string`: `ToString`.

Most of the generated traits normally just include the trait boilerplate and forward the
arguments to your method. There are a few useful extra special cases:

* `PartialOrd` can also be applied to an `Ord`-shaped function, in which case it wraps the result
  with `Some()` to make it fit. `PartialEq` does the same with a `PartialOrd`- or `Ord`-shaped
  function and returns true if it returns `Ordering::Equal`. This allows you to do `#zoet[(Ord,
  PartialEq, PartialOrd)]` to implement all of them in one go. (You'll also need to `#[derive(Eq)]`
  if you generate `PartialEq`.)
* `Add` can also be applied to an `AddAssign`-shaped function, in which case it generates a trivial
  implementation which mutates its `mut self` and returns it. This applies to all of the other
  operator traits with `OpAssign` variants.

## _Unsupported_ traits

Since this macro turns single functions into traits, there needs to be a 1:1 mapping between a
function and a trait. This means that traits which require more than one function (e.g. `Hasher`)
cannot be sanely supported. Likewise, marker traits like `FusedIterator` are not supported even
though doing so would be trivial to implement, because that's really a job for a derive macro.
Finally, traits which themselves have generic functions like `FromIterator` or `Extend` are not
supported because they are beyond the abilities of `zoet`'s current signature parser.

Additionally, traits which are nightly-only like `Generator` are being avoided since there's no
guarantee that `zoet` will be able to keep track of any future updates.

Feel free to raise an issue or PR on the [mooli/zoet-rs GitHub
repository](https://github.com/mooli/zoet-rs) if you would like these to be added and have
productive suggestions.

## What it generates

A suitable impl is emitted which proxies to your function, such as this:

```rust
# struct Length(usize);
# impl Length {
#   fn add_assign(&mut self, rhs: Self) { self.0 += rhs.0; }
# }
impl ::core::ops::Add<Self> for Length {
    type Output = Length;
    fn add(mut self, rhs: Length) -> Length {
        <Length>::add_assign(&mut self, rhs);
        self
    }
}
impl ::core::ops::AddAssign<Length> for Length {
    fn add_assign(&mut self, rhs: Length) {
        <Length>::add_assign(self, rhs);
    }
}
```

You can use [`cargo-expand`] to check the actual expansion.

As a side-note, this particular generated code may look like infinite recursion at a first glance,
but `<Length>::add_assign` explicitly refers to the method in the inherent impl and there is no
ambiguity. However, human readers may still find it confusing—[the `clippy::same_name_method`
lint](https://rust-lang.github.io/rust-clippy/master/#same_name_method) agrees—and you might like to
consider using a different method name such as `_add_assign` or `add_assign_impl` even if those
names are less aesthetically appealing.

## Gotchas

### … due to Rust macro limitations

You must add `#[zoet]` to your struct's impl block so that the self type of its associated functions
can be determined. This is obviously not necessary (or possible) for free functions as they don't
have a self type.

Macros run before type checking, so the actual type is unknown and macros have to work with just
tokens. Most of the time, it is sufficient to just paste the tokens into the output and let the
compiler work it out, and that's what `zoet` does where possible. If the type is wrong, you should
get a helpful compiler error pointing this out. However, some traits require a bit more than simple
pasting, and for sanity (and/or disambiguation) `zoet` _requires_ that the type has a specific
_name_:

* `PartialOrd`: the function should return `Option`. If not it will `Some`-wrap it as if it was
  `Ord`.
* `Future`: the function parameters must be `Pin` and `Poll` respectively.
* `Iterator`: the function must return `Option`.
* `TryFrom`, `TryInto`, `FromStr`: the function must return `Result`. If only one type parameter is
  given, the name `Error` is used for the error type.

Only the last part of the path is compared, so `eyre::Result`, `crate::Result`,
`::core::result::Result` etc. work just as well as a bare `Result`.

This name-checking is only noted here in case you're doing something very strange with type aliases,
and should not affect sensible code.

### … due to `zoet` design limitations

Generic parameters on the function and/or its inherent impl are all just accumulated and added to
the trait impl's generic parameters, which does the right thing for the vast majority of traits.
However, where a trait's function is itself generic, `zoet` isn't (yet) smart enough to figure out
which of the generic parameter is for the function. As a perfectly good workaround, use an `impl
Trait` parameter instead. So while `Hash` defines its single method as `fn hash<H: Hasher>(&self,
state: &mut H)`, your function needs to have a signature like `fn hash(&self, state: &mut impl
Hasher)`.

Elided lifetimes cannot be used on types which are copied into the generated traits, and the
function will need to be tweaked to have named lifetimes. This mainly affects traits such as
`IntoIterator`, and in such cases you would change the signature from e.g. `fn iter(&self) ->
slice::Iter<T>` to `fn iter<'a>(&'a self) -> slice::Iter<'a, T>`.

### … due to _you_ going overboard

While this macro makes it easy to stamp out loads of core traits, don't go crazy but consider each
trait you add and whether there is a more suitable macro to do the job, or indeed whether that trait
should be added. Here are a few tips for _avoiding_ using this crate:

* The example above generates `Default` based on `new()`, but since that function returns 0 which is
  the default value anyway, it is better to `#derive(Default)` and implement `new()` in terms of
  that.
* Similarly, its `Add`- and `AddAssign`-adorned functions are trivial delegations to its field's
  `Add` and `AddAssign` traits. The [`derive_more`] crate handles this and will reduce the amount of
  boilerplate further, and in this case a simple `#[derive(Add, AddAssign)]` on the struct will
  replace those functions.
* [`educe`] lets you derive and customise `Debug`, `Default`, `Hash`, `Clone`, and `Copy` without
  writing actual boilerplate functions.
* `Borrow` is not just a synonym for `AsRef`, but gives specific guarantees, notably that "`Eq`,
  `Ord` and `Hash` must be equivalent for borrowed and owned values". If your `AsRef` doesn't offer
  those guarantees, don't write `#[zoet(AsRef, Borrow, Deref)]`.

[`cargo-expand`]: https://crates.io/crates/cargo-expand
[`derive_more`]: https://crates.io/crates/derive_more
[`educe`]: https://crates.io/educe
