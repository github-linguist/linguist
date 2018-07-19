: str-eq  ( str len str len -- ? ) compare 0= ;
: str-neq ( str len str len -- ? ) compare 0<> ;
: str-lt  ( str len str len -- ? ) compare 0< ;
: str-gt  ( str len str len -- ? ) compare 0> ;
: str-le  ( str len str len -- ? ) compare 0<= ;
: str-ge  ( str len str len -- ? ) compare 0>= ;
