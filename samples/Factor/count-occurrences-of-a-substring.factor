USING: math sequences splitting ;
: occurences ( seq subseq -- n ) split-subseq length 1 - ;
