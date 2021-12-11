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

// And the same, but with lifetimes.

struct Slice<'a, T>(&'a mut [T]);
#[zoet]
impl<'a, T> Slice<'a, T> {
    #[zoet(IntoIterator)]
    fn iter(&'a self) -> std::slice::Iter<'a, T> {
        self.0.iter()
    }

    #[zoet(IntoIterator)]
    fn iter_mut(&'a mut self) -> std::slice::IterMut<'a, T> {
        self.0.iter_mut()
    }

    // TODO: surprisingly tricky to implement
    // #[zoet(FromIterator)]
    // fn from_iter(ii: impl IntoIterator<Item = T>) -> Self {
    // Self(ii.into_iter().collect())
    // }
}
