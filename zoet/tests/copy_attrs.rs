use zoet::zoet;

// Check that #[cfg] is applied to the generated trait; if it's not, we will get a compile-time
// failure complaining about conflicting (i.e. duplicate) implementations of Default.

struct Foo;
#[zoet]
impl Foo {
    #[cfg(unix)]
    #[zoet(Default)]
    fn unix_foo() -> Foo {
        Foo
    }

    #[cfg(not(unix))]
    #[zoet(Default)]
    fn not_unix_foo() -> Foo {
        Foo
    }
}
