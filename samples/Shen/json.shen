(load "grammar.shen")

\*

JSON Lexer

1. Read a stream of characters
2. Whitespace characters not in strings should be discarded.
3. Whitespace characters in strings should be preserved
4. Strings can contain escaped double quotes. e.g. "\""

*\

(define whitespacep
  \* e.g. ASCII 32 == #\Space. *\
  \* All the others are whitespace characters from an ASCII table. *\
  Char -> (member Char ["c#9;" "c#10;" "c#11;" "c#12;" "c#13;" "c#32;"]))

(define replace-whitespace
  "" -> ""
  (@s Whitespace Suffix) -> (@s "" (replace-whitespace Suffix)) where (whitespacep Whitespace)
  (@s Prefix Suffix) -> (@s Prefix (replace-whitespace Suffix)))

(define fetch-until-unescaped-doublequote
  [] -> []
  ["\" "c#34;" | Chars] -> ["\" "c#34;" | (fetch-until-unescaped-doublequote Chars)]
  ["c#34;" | Chars] -> []
  [Char | Chars] -> [Char | (fetch-until-unescaped-doublequote Chars)])

\* (define strip-whitespace-chars *\
\*   [] -> [] *\
\*   ["c#34;" | Chars] -> ["c#34;" | ( *\
\*   [WhitespaceChar | Chars] -> (strip-whitespace-chars Chars) where (whitespace? WhitespaceChar) *\
\*   [Char | Chars] -> [Char | (strip-whitespace-chars Chars)]) *\

(define tokenise
  JSONString ->
  (let CharList (explode JSONString)
       CharList))