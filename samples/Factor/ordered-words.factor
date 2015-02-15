USING: grouping http.client io io.encodings.utf8 io.files
io.files.temp kernel math memoize sequences sequences.extras
unicode.case urls ;
IN: rosetta-code.ordered-words

MEMO: word-list ( -- seq )
    "unixdict.txt" temp-file dup exists? [
        URL" http://puzzlers.org/pub/wordlists/unixdict.txt"
        over download-to
    ] unless utf8 file-lines ;

: ordered-word? ( word -- ? )
    >lower [ <= ] monotonic? ;

: ordered-words-main ( -- )
    word-list [ ordered-word? ] filter
    all-longest [ print ] each ;
