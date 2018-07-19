: levenshtein                          ( a1 n1 a2 n2 -- n3)
  dup                                  \ if either string is empty, difference
  if                                   \ is inserting all chars from the other
    2>r dup
    if
      2dup 1- chars + c@ 2r@ 1- chars + c@ =
      if
        1- 2r> 1- recurse exit
      else                             \ else try:
        2dup 1- 2r@ 1- recurse -rot    \   changing first letter of s to t;
        2dup    2r@ 1- recurse -rot    \   remove first letter of s;
        1- 2r> recurse min min 1+      \   remove first letter of t,
      then                             \ any of which is 1 edit plus
    else                               \ editing the rest of the strings
      2drop 2r> nip
    then
  else
    2drop nip
  then
;

s" kitten"      s" sitting"       levenshtein . cr
s" rosettacode" s" raisethysword" levenshtein . cr
