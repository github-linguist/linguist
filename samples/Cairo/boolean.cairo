#[generate_trait]
pub impl BoolImpl<T, +Drop<T>> of BoolTrait<T> {
    /// Returns `Some(t)` if the `bool` is `true`, or `None` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(false.then_some(0) == Option::None);
    /// assert!(true.then_some(0) == Option::Some(0));
    /// ```
    #[inline(always)]
    fn then_some(self: bool, t: T) -> Option<T> nopanic {
        if self {
            Option::Some(t)
        } else {
            Option::None
        }
    }
}
