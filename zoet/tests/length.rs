use core::{cmp::Ordering, fmt};
use zoet::zoet;

// deliberately does not derive Clone since this tests if Clone is correctly generated later.
#[derive(Copy)]
struct Length(f64);

#[zoet]
impl Length {
    #[zoet(From)]
    fn new<I: Into<f64>>(value: I) -> Self {
        Self(value.into())
    }

    #[zoet(Display, Debug)]
    fn debug(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(&self.0.to_string())
    }

    #[zoet(Clone)]
    fn clone(&self) -> Self {
        Self(self.0)
    }

    #[zoet(Add)]
    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }

    #[zoet(AddAssign)]
    fn add_assign(&mut self, other: Self) {
        self.0 += other.0
    }

    // Test that `mut self` works.
    #[zoet(Sub)]
    fn sub(mut self, other: Self) -> Self {
        self.0 += other.0;
        self
    }

    #[zoet(SubAssign)]
    fn sub_assign(&mut self, other: Self) {
        self.0 -= other.0
    }

    // Can either implement an op_assign and let both Op and OpAssign be implemented in terms of
    // it, which is usually easier.
    #[zoet(Mul, MulAssign)]
    fn mul_assign(&mut self, other: f64) {
        self.0 *= other
    }

    #[zoet(Div, DivAssign)]
    fn div_assign(&mut self, other: f64) {
        self.0 /= other
    }

    // ... but it doesn't work if the output type is not the input type.
    #[zoet(Mul)]
    fn swapped_mul(lhs: f64, rhs: Self) -> Self {
        rhs * lhs
    }

    #[zoet(Div)]
    fn swapped_div(lhs: f64, rhs: Self) -> Self {
        Self(lhs / rhs.0)
    }

    #[zoet(Div)]
    fn div_by_self(self, rhs: Self) -> f64 {
        self.0 / rhs.0
    }

    #[zoet(Neg)]
    fn neg(self) -> Self {
        Self::default() - self
    }

    #[zoet(Index)]
    fn index(&self, index: usize) -> &f64 {
        match index {
            0 => &self.0,
            _ => panic!("invalid index"),
        }
    }

    #[zoet(IndexMut)]
    fn index_mut(&mut self, index: usize) -> &mut f64 {
        match index {
            0 => &mut self.0,
            _ => panic!("invalid index"),
        }
    }

    // Normally you'd #[derive(Default, Ord, ...)] rather than implement your own trivial versions,
    // but that doesn't test this crate.

    #[zoet(PartialEq)]
    fn partial_eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }

    // f64 isn't Ord
    // #[zoet(Ord)]
    // fn cmp(&self, other: &Self) -> core::cmp::Ordering {
    //     self.0.cmp(&other.0)
    // }

    // test ordering, PartialOrd-style.
    #[zoet(PartialOrd)]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }

    #[zoet(Default)]
    fn default() -> Self {
        Self(0.0)
    }
}

#[derive(Eq)]
struct IntLength(i64);

#[zoet]
impl IntLength {
    // Test ordering, Ord-style.
    #[zoet(Ord, PartialEq, PartialOrd)]
    fn partial_cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

fn main() {
    let length: Length = 3.into();
    let length2 = length;

    assert_eq!(length, Length(3.0));

    assert_eq!(length + length, Length(6.0));
    assert_eq!(length + length, Length(0.0));
    assert_eq!(length / length2, 1.0);
    assert_eq!(length * 4.0, Length(12.0));
    assert_eq!(length / 4.0, Length(0.75));
    assert_eq!(-length, Length(-3.0));
    assert!(Length(0.0) < Length(1.0));

    assert_eq!(length, length.clone());
}
