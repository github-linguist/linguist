( Block words. )

variable blk
variable current-block

: block ( n -- addr )
    current-block ! 0 ;

: buffer ( n -- addr )
    current-block ! 0 ;

\ evaluate (extended semantics)
\ flush ( -- )

: load ( ... n -- ... )
    dup current-block !
    blk !
    save-input
    0 >in !
    blk @ block ''source !  1024 ''#source !
    ( interpret )
    restore-input ;

\ save-buffers ( -- )
\ update ( -- )

( Block extension words. )

\ empty-buffers ( -- )

variable  scr

: list ( n -- )
    dup scr !
    dup current-block !
    block 1024 bounds do i @ emit loop ;

\ refill (extended semantics)

: thru ( x y -- )   +1 swap do i load loop ;

\ \ (extended semantics)
