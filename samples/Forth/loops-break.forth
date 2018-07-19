include random.fs

: main
  begin  20 random dup . 10 <>
  while  20 random .
  repeat ;

\ use LEAVE to break out of a counted loop
: main
  100 0 do
    i random dup .
    10 = if leave then
    i random .
  loop ;
