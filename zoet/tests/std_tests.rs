use zoet::zoet;

#[repr(transparent)]
struct Foo(str);
struct FooBuf(String);

#[zoet]
impl Foo {
    #[zoet(ToOwned)]
    fn to_owned(&self) -> FooBuf {
       FooBuf(self.0.to_owned())
    }

    #[zoet(ToString)]
    fn to_string(&self) -> String {
        self.0.to_owned()
    }
}

#[zoet]
impl FooBuf {
    #[zoet(Borrow)]
    fn as_ref(&self) -> &Foo {
        let s: &str = &self.0;
        unsafe { &*(s as *const str as *const Foo) }
    }
}
