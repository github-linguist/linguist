/**
This is a documentation comment for someFunc and someFunc2.
$(DDOC_COMMENT comment inside a documentation comment
(results in a HTML comment not displayed by the browser))

Header:
    content (does not need to be tabbed out; this is done for clarity
    of the comments and has no effect on the resulting documentation)

Params:
    arg1 = Something (listed as "int <i>arg1</i> Something")
    arg2 = Something else

Returns:
    Nothing

TODO:
    Nothing at all

BUG:
    None found
*/
void someFunc(int arg1, int arg2) {}

// This groups this function with the above (both have the
//  same doc and are listed together)
/// ditto
void someFunc2(int arg1, int arg2) {}

/// Sum function.
int sum(in int x, in int y) pure nothrow {
    return x + y;
}

// These unittests will become part of sum documentation:
///
unittest {
    assert(sum(2, 3) == 5);
}

/++ Another documentation comment +/
void main() {}
