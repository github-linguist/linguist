// A single line comment

/*
    This is a multi-line (aka block) comment

    /*
        containing nested multi-line comment
        (nesting supported since 0.9-pre https://github.com/mozilla/rust/issues/9468)
    */
*/


/// Outer single line Rustdoc comments apply to the next item.

/**
    Outer multi-line Rustdoc comments.

 *  Leading asterisk (*) in multi-line Rustdoc comments
 *  is not considered to be part of the comment text,
 *  blanks and tabs preceding the initial asterisk (*) are also stripped.
*/

fn example() {

    //! Inner single line Rustdoc comments apply to their enclosing item.

    /*!
        Inner multi-line Rustdoc comments.
        See also https://github.com/mozilla/rust/wiki/Doc-using-rustdoc
    */
}

#[doc = "Unsugared outer Rustdoc comments.
        (outer attributes are not terminated by a semi-colon)"]
fn example() {
    #[doc = "Unsugared inner Rustdoc comments.
            (inner attributes are terminated by a semi-colon)
            See also https://github.com/mozilla/rust/blob/master/doc/rust.md#attributes"];
}
