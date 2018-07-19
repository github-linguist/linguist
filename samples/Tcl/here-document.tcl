set hereDocExample {
In Tcl, the {curly brace} notation is strictly a here-document style notation
as it permits arbitrary content inside it *except* for an unbalanced brace.
That is typically not a problem as seen in reality, as almost all content that
might be placed in a here-doc is either brace-free or balanced. The content
of the braces is not interpreted at all; no substitutions are performed on it.

The sole exception is that there is limited processing of backslashes; a single
backslash at the end of a line causes the end-of-line plus all whitespace at
the start of the next line to be compressed to a single space.
}
