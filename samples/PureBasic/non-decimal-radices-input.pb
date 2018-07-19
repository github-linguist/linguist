  ;Val() parses integer strings
  ; decimal numbers have no prefix, hexadecimal needs a prefix of '$', binary needs a prefix of '%'
  Val("1024102410241024")      ; => 1024102410241024
  Val("$10FFFFFFFF")           ; => 73014444031
  Val("%1000")                 ; => 8
