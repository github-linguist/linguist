USING: classes kernel prettyprint serialize ;
TUPLE: A ;
TUPLE: C < A ;
: serial-clone ( obj -- obj' ) object>bytes bytes>object ;

C new
[ clone ]
[ serial-clone ] bi [ class . ] bi@
