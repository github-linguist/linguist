MyVar = "This is the text inside MyVar"
MyVariable =
(
   Note that whitespace is preserved
   As well as newlines.
   The LTrim option can be present to remove left whitespace.
   Variable references such as %MyVar% are expanded.
)
MsgBox % MyVariable
