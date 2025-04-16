digit                       [0-9]
id                          [a-zA-Z][a-zA-Z0-9]*

%%
"//".*                      /* ignore comment */
"main"                      return 'MAIN';
"class"                     return 'CLASS';
"extends"                   return 'EXTENDS';
"nat"                       return 'NATTYPE';
"if"                        return 'IF';
"else"                      return 'ELSE';
"for"                       return 'FOR';
"printNat"                  return 'PRINTNAT';
"readNat"                   return 'READNAT';
"this"                      return 'THIS';
"new"                       return 'NEW';
"var"                       return 'VAR';
"null"                      return 'NUL';
{digit}+                    return 'NATLITERAL';
{id}                        return 'ID';
"=="                        return 'EQUALITY';
"="                         return 'ASSIGN';
"+"                         return 'PLUS';
"-"                         return 'MINUS';
"*"                         return 'TIMES';
">"                         return 'GREATER';
"||"                        return 'OR';
"!"                         return 'NOT';
"."                         return 'DOT';
"{"                         return 'LBRACE';
"}"                         return 'RBRACE';
"("                         return 'LPAREN';
")"                         return 'RPAREN';
";"                         return 'SEMICOLON';
\s+                         /* skip whitespace */
"."                         throw 'Illegal character';
<<EOF>>                     return 'ENDOFFILE';


