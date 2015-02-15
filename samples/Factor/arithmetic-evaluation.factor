USING: accessors kernel locals math math.parser peg.ebnf ;
IN: rosetta.arith

TUPLE: operator left right ;
TUPLE: add < operator ;   C: <add> add
TUPLE: sub < operator ;   C: <sub> sub
TUPLE: mul < operator ;   C: <mul> mul
TUPLE: div < operator ;   C: <div> div

EBNF: expr-ast
spaces   = [\n\t ]*
digit    = [0-9]
number   = (digit)+                         => [[ string>number ]]

value    =   spaces number:n                => [[ n ]]
           | spaces "(" exp:e spaces ")"    => [[ e ]]

fac      =   fac:a spaces "*" value:b       => [[ a b <mul> ]]
           | fac:a spaces "/" value:b       => [[ a b <div> ]]
           | value

exp      =   exp:a spaces "+" fac:b         => [[ a b <add> ]]
           | exp:a spaces "-" fac:b         => [[ a b <sub> ]]
           | fac

main     = exp:e spaces !(.)                => [[ e ]]
;EBNF

GENERIC: eval-ast ( ast -- result )

M: number eval-ast ;

: recursive-eval ( ast -- left-result right-result )
    [ left>> eval-ast ] [ right>> eval-ast ] bi ;

M: add eval-ast recursive-eval + ;
M: sub eval-ast recursive-eval - ;
M: mul eval-ast recursive-eval * ;
M: div eval-ast recursive-eval / ;

: evaluate ( string -- result )
    expr-ast eval-ast ;
