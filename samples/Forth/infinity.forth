: inf ( -- f ) 1e 0e f/ ;
inf f.    \ implementation specific. GNU Forth will output "inf"

: inf? ( f -- ? ) s" MAX-FLOAT" environment? drop f> ;
\ IEEE infinity is the only value for which this will return true

: has-inf ( -- ? ) ['] inf catch if false else inf? then ;
