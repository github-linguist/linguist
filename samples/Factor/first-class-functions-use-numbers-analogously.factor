USING: arrays kernel literals math prettyprint sequences ;
IN: q

CONSTANT: x  2.0
CONSTANT: xi 0.5
CONSTANT: y  4.0
CONSTANT: yi .25
CONSTANT: z $[ $ x $ y + ]
CONSTANT: zi $[ 1 $ x $ y + / ]

CONSTANT: A ${ x y z }
CONSTANT: B ${ xi yi zi }

: multiplier ( n1 n2 -- q ) [ * * ] 2curry ;
: create-all ( seq1 seq2 -- seq ) [ multiplier ] 2map ;
: example  ( -- )
  0.5 A B create-all
  [ call( x -- y ) ] with map . ;
