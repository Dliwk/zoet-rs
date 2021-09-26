mod std_result {
    struct Foo;
    struct MyError;

    #[zoet::zoet(TryFrom)]
    fn test(_foo: Foo) -> Result<(), MyError> {
        Ok(())
    }
}

mod typedef_result {
    type Result<T> = core::result::Result<T, Error>;
    struct Foo;
    struct Error;

    #[zoet::zoet(TryFrom)]
    fn test(_foo: Foo) -> Result<()> {
        Ok(())
    }
}
