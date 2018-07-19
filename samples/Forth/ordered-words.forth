include lib/stmstack.4th               \ include string stack library

: check-word                           ( a n -- a n f)
  2dup bl >r                           \ start off with a space
  begin
    dup                                \ when not end of word
  while
    over c@ r@ >=                      \ check character
  while
    r> drop over c@ >r chop            \ chop character off
  repeat r> drop nip 0=                \ cleanup and set flag
;

: open-file                            ( -- h)
  1 dup argn = abort" Usage: ordered infile"
  args input open error? abort" Cannot open file"
  dup use                              \ return and use the handle
;

: read-file                            ( --)
  0 >r                                 \ begin with zero length
  begin
    refill                             \ EOF detected?
  while
    0 parse dup r@ >=                  \ equal or longer string length?
    if                                 \ check the word and adjust length
      check-word if r> drop dup >r >s else 2drop then
    else                               \ if it checks out, put on the stack
      2drop                            \ otherwise drop the word
    then
  repeat r> drop                       \ clean it up
;

: read-back                            ( --)
  s> dup >r type cr                    \ longest string is on top of stack
  begin s> dup r@ >= while type cr repeat
  2drop r> drop                        \ keep printing until shorter word
;                                      \ has been found

: ordered                              ( --)
  open-file s.clear read-file read-back close
;                                      \ open file, clear the stack, read file
                                       \ read it back and close the file
ordered
