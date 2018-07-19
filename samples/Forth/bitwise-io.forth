\ writing

: init-write ( -- b m ) 0 128 ;

: flush-bits ( b m -- 0 128 ) drop emit init-write ;

: ?flush-bits ( b m -- b' m' ) dup 128 < if flush-bits then ;

: write-bit ( b m f -- b' m' )
  if tuck or swap then
  2/ dup 0= if flush-bits then ;

\ reading

: init-read ( -- b m ) key 128 ;

: eof? ( b m -- b m f ) dup if false else key? 0= then ;

: read-bit ( b m -- b' m' f )
  dup 0= if 2drop init-read then
  2dup and swap 2/ swap ;
