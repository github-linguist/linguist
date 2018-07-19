H{ { "one" 1 } { "two" 2 } }
{ [ "one" swap at . ]
  [ 2 swap value-at . ]
  [ "three" swap at . ]
  [ [ 3 "three" ] dip set-at ]
  [ "three" swap at . ] } cleave
