dnl Took from https://en.wikipedia.org/wiki/M4_(computer_language)
divert(-1)

M4 has multiple output queues that can be manipulated with the
`divert' macro. Valid queues range from 0 to 10, inclusive, with
the default queue being 0.

Calling the `divert' macro with an invalid queue causes text to be
discarded until another call.  Note that even while output is being
discarded, quotes around `divert' and other macros are needed to
prevent expansion.

# Macros aren't expanded within comments, meaning that keywords such
# as divert and other built-ins may be used without consequence.

# HTML utility macro:

define(`H2_COUNT', 0)

# The H2_COUNT macro is redefined every time the H2 macro is used:

define(`H2',
	`define(`H2_COUNT', incr(H2_COUNT))<h2>H2_COUNT. $1</h2>')

divert(1)dnl
dnl
dnl The dnl macro causes m4 to discard the rest of the line, thus
dnl preventing unwanted blank lines from appearing in the output.
dnl
H2(First Section)
H2(Second Section)
H2(Conclusion)
dnl
divert(0)dnl
dnl
<HTML>
undivert(1)dnl One of the queues is being pushed to output.
</HTML>
