: take-shorter ( seq1 seq2 -- shorter )
    [ shorter? ] 2keep ? ;

: common-head ( seq1 seq2 -- head )
    2dup mismatch [ nip head ] [ take-shorter ] if* ;

: common-prefix-1 ( file1 file2 separator -- prefix )
    [ common-head ] dip '[ _ = not ] trim-tail ;

: common-prefix ( seq separator -- prefix )
    [ ] swap '[ _ common-prefix-1 ] map-reduce ;
