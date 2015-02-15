USING: assocs fry io.encodings.utf8 io.files kernel math
math.combinatorics sequences sorting strings ;
IN: rosettacode.deranged-anagrams

: derangement? ( str1 str2 -- ? ) [ = not ] 2all? ;
: derangements ( seq -- seq )
    2 [ first2 derangement? ] filter-combinations ;

: parse-dict-file ( path -- hash )
    utf8 file-lines
    H{ } clone [
        '[
            [ natural-sort >string ] keep
            _ [ swap suffix  ] with change-at
        ] each
    ] keep ;

: anagrams ( hash -- seq ) [ nip length 1 > ] assoc-filter values ;

: deranged-anagrams ( path -- seq )
    parse-dict-file anagrams [ derangements ] map concat ;

: longest-deranged-anagrams ( path -- anagrams )
    deranged-anagrams [ first length ] sort-with last ;
