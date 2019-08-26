mod failure_fallible {
    type Fallible<T> = core::result::Result<T, Error>;
    struct Foo;
    struct Error;

    #[zoet::zoet(TryFrom)]
    fn test(_foo: Foo) -> Fallible<()> { Ok(()) }
}

mod std_result {
    struct Foo;
    struct Error;

    #[zoet::zoet(TryFrom)]
    fn test(_foo: Foo) -> Result<(), Error> { Ok(()) }
}

mod typedef_result {
    type Result<T> = core::result::Result<T, Error>;
    struct Foo;
    struct Error;

    #[zoet::zoet(TryFrom)]
    fn test(_foo: Foo) -> Result<()> { Ok(()) }
}
