: .bool ( ? -- ) if ." true" else ." false" then ;
: logic ( a b -- ) 0<> swap 0<> swap
 cr ." a = " over .bool ."   b = " dup .bool
 cr ." a and b = " 2dup and .bool
 cr ." a  or b = " over  or .bool
 cr ." not a = " 0= .bool ;
