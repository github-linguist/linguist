\ <to> <from> copy-file
: copy-file ( a1 n1 a2 n2 -- )
    r/o open-file throw >r
    w/o create-file throw r>
    begin
        pad maxstring  2 pick  read-file throw
    ?dup while
        pad swap  3 pick  write-file throw
    repeat
    close-file throw
    close-file throw ;

\ Invoke it like this:
s" output.txt" s" input.txt" copy-file
