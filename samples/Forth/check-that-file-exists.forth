: .exists ( str len -- ) 2dup file-status nip 0= if type ."  exists" else type ."  does not exist" then ;
 s" input.txt" .exists
s" /input.txt" .exists
 s" docs" .exists
s" /docs" .exists
