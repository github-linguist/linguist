USING: math sequences kernel ;

! starting from n characters in and of m length
: subseq* ( from length seq -- newseq ) [ over + ] dip subseq ;

! starting from n characters in, up to the end of the string
: dummy ( seq n -- tailseq ) tail ;

! whole string minus last character
: dummy1 ( seq -- headseq ) but-last ;

USING: fry sequences kernel ;
! helper word
: subseq-from-* ( subseq len seq quot -- seq ) [ nip ] prepose 2keep subseq* ; inline

! starting from a known character within the string and of m length;
: subseq-from-char ( char len seq -- seq ) [ index ] subseq-from-* ;

! starting from a known substring within the string and of m length.
: subseq-from-seq ( subseq len seq -- seq ) [ start ] subseq-from-* ;
