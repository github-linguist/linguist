USING: arrays assocs concurrency.combinators
concurrency.semaphores formatting hashtables http.client io
json.reader kernel math math.parser sequences splitting
urls.encoding ;
IN: rosetta-code.count-examples

CONSTANT: list-url "http://rosettacode.org/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&cmprop=title&format=json"

: titles ( query -- titles )
  "query" of "categorymembers" of [ "title" of ] map ;
: continued-url ( query -- url/f )
  "query-continue" of "categorymembers" of
  [ assoc>query list-url swap "&" glue ] [ f ] if* ;

: (all-programming-titles) ( titles url -- titles' url' )
  http-get nip json> [ titles append ] [ continued-url ] bi
  [ (all-programming-titles) ] [ f ] if* ;
: all-programming-titles ( -- titles ) { } list-url (all-programming-titles) drop ;

CONSTANT: content-base-url "http://rosettacode.org/mw/index.php?title=&action=raw"
: content-url ( title -- url )
  " " "_" replace
  "title" associate assoc>query
  content-base-url swap "&" glue ;

: occurences ( seq subseq -- n ) split-subseq length 1 - ;
: count-examples ( title -- n )
  content-url http-get nip "=={{header|" occurences ;

: print-task ( title n -- ) "%s: %d examples.\n" printf ;
: print-total ( assoc -- ) values sum "Total: %d examples.\n" printf ;
: fetch-counts ( titles -- assoc )
  10 <semaphore> [
    [ dup count-examples 2array ] with-semaphore
  ] curry parallel-map ;

: print-counts ( titles -- )
  [ [ print-task ] assoc-each nl ] [ print-total ] bi ;
: rosetta-examples ( -- )
  all-programming-titles fetch-counts print-counts ;

MAIN: rosetta-examples
