include ffl/rgx.fs

\ Create a regular expression variable 'exp' in the dictionary

rgx-create exp

\ Compile an expression

s" Hello (World)" exp rgx-compile [IF]
  .( Regular expression successful compiled.) cr
[THEN]

\ (Case sensitive) match a string with the expression

s" Hello World" exp rgx-cmatch? [IF]
  .( String matches with the expression.) cr
[ELSE]
  .( No match.) cr
[THEN]
