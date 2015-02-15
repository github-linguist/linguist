: factors dup 2/ 1+ 1 do dup i mod 0= if i swap then loop ;
: .factors factors begin dup dup . 1 <> while drop repeat drop cr ;

45 .factors
53 .factors
64 .factors
100 .factors
