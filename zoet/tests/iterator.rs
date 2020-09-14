use zoet::zoet;

struct Array<T>(Vec<T>);

#[zoet]
impl<T> Array<T> {
    #[zoet(IntoIterator)]
    fn into_iterator(self) -> std::vec::IntoIter<T> {
        self.0.into_iter()
    }

    // TODO: surprisingly tricky to implement
    // #[zoet(FromIterator)]
    // fn from_iter(ii: impl IntoIterator<Item = T>) -> Self {
    // Self(ii.into_iter().collect())
    // }
}
