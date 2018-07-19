# Nimrod supports only single-line comments

var x = 0 ## Documentation comments start with double hash characters.

var y = 0 ## Comments are a proper part of the syntax and are indentation sensitive.
          ## If the next line only consists of a comment piece, it must be aligned
          ## either to the preceding one (documentation generator would merge these lines)
## or to the code block (starting a new comment piece).

  # any misaligned comment line (like this one) would trigger an "invalid indentation" error

var z = 0 ## Alternatively you can end the preceding comment piece with a backslash \
  ## to reset alignment to any column you like.
  ## See also http://nimrod-code.org/tut1.html#comments
