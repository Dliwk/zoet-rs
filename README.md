Adds `#[zoet]` macro to reduce boilerplate when implementing common traits.

If you are sick of writing `impl Deref for Bar` etc. and it didn't compile because you confused it
with `AsRef`, had a hard-to-debug problem because you implemented `PartialOrd` and mistakenly
thought that deriving `Ord` would do the sane thing, and/or you would rather just implement these
core traits as regular methods in your `impl Bar` like lesser languages, this crate is for you!

See the [documentation on crates.io](https://docs.rs/zoet) for the full list of supported
traits, but here's a taster:

```rust
use core::cmp::Ordering;
use zoet::zoet;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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
    fn ord(&self, other: &Self) -> Ordering { self.0.cmp(&other.0) }
}

let mut v = Length::default();
v += Length(1);
assert_eq!(v + Length(2), Length(3));
v += Length(4);
assert_eq!(v, Length(5));
assert_eq!(Length::from(v), Length(5));
```
