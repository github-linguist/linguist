\ GForth specific words:
\ under+ ( a b c -- a+c b) , latest ( -- nt ) , name>string ( nt -- ca u )
\ Should not be a problem to modify it to work with other Forth implementation:

: $!   ( ca u -- a )
  dup >R dup , here swap move R> allot ;
: $@   ( a -- ca u )
  dup @ 1 cells under+ ;
: c!+   ( c ca - ca+1 )
  tuck c! 1+ ;
: $!+   ( a u a' -- a'+u ; string-store-plus )
  2dup + >R swap move R> ;

\ --- UNIX end-of-line adapted words
10 constant EOL
: input-fix   ( -- ; for interactive use ! )
  source-id 0= IF cr THEN ;
: get_line  ( -- ca u )  EOL parse input-fix ;
: EOL!+  ( a -- a' ; eol-store-plus )  EOL swap c!+ ;

: EOD   ( -- ca u ; end-of-doc )
  latest name>string ;

: >>   ( "doc-name" "doc-body" -- )   input-fix
  CREATE 0 ,              \ post body length
  here dup >R
  BEGIN  refill >R
         get_line 2dup EOD compare
         R> AND	          \ notEOD && input-stream ->
  WHILE  rot $!+ EOL!+
  REPEAT 2drop
  R> tuck - dup allot
  swap -1 cells + !       \ fixup body length
  DOES>  ( -- ca u )  $@ ;

\ TEST ;   excerpt from Project Gutenberg 'Alice in Wonderland'

>> ALICE
CHAPTER I. Down the Rabbit-Hole

Alice was beginning to get very tired of sitting by her sister on the
bank, and of having nothing to do: once or twice she had peeped into the
book her sister was reading, but it had no pictures or conversations in
it, 'and what is the use of a book,' thought Alice 'without pictures or
conversation?'
ALICE
>> RABBIT
RABBIT
ALICE type ." --" cr RABBIT type
