report zdiv_zero
data x type i.
try.
  x = 1 / 0.
catch CX_SY_ZERODIVIDE.
  write 'Divide by zero.'.
endtry.
