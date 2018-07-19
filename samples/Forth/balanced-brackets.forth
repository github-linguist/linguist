include lib/choose.4th                 ( n1 -- n2)
include lib/ctos.4th                   ( n -- a 1)

 10 constant /[]                       \ maximum number of brackets
/[] string    []                       \ string with brackets
                                       \ create string with brackets
: make[]                               ( --)
  0 dup [] place /[] choose 0 ?do 2 choose 2* [char] [ + c>s [] +place loop
;                                      \ empty string, fill with brackets
                                       \ evaluate string
: eval[]                               ( --)
  [] count 2dup over chars + >r swap type 0
  begin                                \ setup string and count
    over r@ <                          \ reached end of string?
  while                                \ if not ..
    dup 0< 0=                          \ unbalanced ]?
  while                                \ if not ..
    over c@ [char] \ - negate + swap char+ swap
  repeat                               \ evaluate, goto next character
  r> drop if ."  NOT" then ."  OK" cr drop
;                                      \ evaluate string and print result

make[] eval[]
