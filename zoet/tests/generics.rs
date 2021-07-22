use core::hash::{Hash, Hasher};
use zoet::zoet;

#[derive(Eq, PartialEq)]
struct Length<I> {
    value: I,
}

#[zoet]
impl<I> Length<I> {
    // easy generics

    #[zoet(From)]
    fn from(value: I) -> Self {
        Self { value }
    }

    // not-so-easy generics

    #[zoet(Hash)]
    fn hash(&self, state: &mut impl Hasher)
    where
        I: Hash,
    {
        self.value.hash(state)
    }
}

// free function generics. You can se how this gets ugly quickly.

#[zoet(Default)]
fn len_default<I: Default>() -> Length<I> {
    I::default().into()
}

#[test]
fn test() {
    let a = Length { value: 123 };
    let b = Length::from(123);
    let _d = <Length<u128>>::default();
    let mut h = std::collections::HashMap::<_, _>::new();
    h.insert(a, b);
}
