## Copyright (C)
## octave functions usuually start with Copyright or Author (this is part of the parser)

## -*- texinfo -*-
## and also followed by documentatino on texinfo

function ret = octave_function(A,B)
  # Simple function adding two values and displaying the return value

  ret = A' + B';  # make sure we don't confuse the transpose operator with strings
  % Display the return value
  disp("Return value in function");
  disp(ret);

endfunction
