2 constant Pos
3 constant Len
: Str ( -- c-addr u )  s" abcdefgh" ;

Str Pos /string drop Len type    \ cde
Str Pos /string type             \ cdefgh
Str 1- type                      \ abcdefg
Str char d scan drop Len type    \ def
Str s" de" search 2drop Len type \ def
