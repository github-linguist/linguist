require(a == 42)          # default message, "Required condition failed"

require(a == 42, "The Answer is Wrong.")   # supplied message

require(a == 42, fn { `Off by ${a - 42}.` })   # computed only on failure
