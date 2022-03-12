#![cfg(feature = "alloc")] // this tests traits which require an allocator

use zoet::zoet;

#[repr(transparent)]
struct Foo(str);
struct FooBuf(String);

#[zoet]
impl Foo {
    fn new(s: &str) -> &Self {
        unsafe { &*(s as *const str as *const Foo) }
    }

    #[zoet(ToOwned)]
    fn to_owned(&self) -> FooBuf {
        FooBuf(self.0.to_owned())
    }

    #[zoet(ToString)]
    fn zoet_to_string(&self) -> String {
        self.0.to_owned()
    }
}

#[zoet]
impl FooBuf {
    #[zoet(Borrow)]
    fn as_ref(&self) -> &Foo {
        Foo::new(&self.0)
    }
}
