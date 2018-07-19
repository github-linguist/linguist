SYMBOL: foo

: use-foo ( -- )
    1 foo set
    foo get 2 + foo set ! foo now = 3
    foo get number>string print ;

:: named-param-example ( a b -- )
    a b + number>string print ;

: local-example ( -- str ) [let "a" :> b "c" :> a a " " b 3append ] ;
