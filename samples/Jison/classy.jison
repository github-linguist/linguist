
/* description: ClassyLang grammar. Very classy. */
/*
  To build parser:

    $ ./bin/jison examples/classy.jison examples/classy.jisonlex

*/


/* author: Zach Carter */

%right ASSIGN
%left OR
%nonassoc EQUALITY GREATER
%left PLUS MINUS
%left TIMES
%right NOT
%left DOT

%%

pgm
    : cdl MAIN LBRACE vdl el RBRACE ENDOFFILE
    ;

cdl
    : c cdl
    |
    ;

c
    : CLASS id EXTENDS id LBRACE vdl mdl RBRACE
    ;

vdl
    : VAR t id SEMICOLON vdl
    |
    ;

mdl
    : t id LPAREN t id RPAREN LBRACE vdl el RBRACE mdl
    |
    ;

t
    : NATTYPE
    | id
    ;

id
    : ID
    ;

el
    : e SEMICOLON el
    | e SEMICOLON
    ;

e
    : NATLITERAL
    | NUL
    | id
    | NEW id
    | THIS
    | IF LPAREN e RPAREN LBRACE el RBRACE ELSE LBRACE el RBRACE
    | FOR LPAREN e SEMICOLON e SEMICOLON e RPAREN LBRACE el RBRACE
    | READNAT LPAREN RPAREN
    | PRINTNAT LPAREN e RPAREN
    | e PLUS e
    | e MINUS e
    | e TIMES e
    | e EQUALITY e
    | e GREATER e
    | NOT e
    | e OR e
    | e DOT id
    | id ASSIGN e
    | e DOT id ASSIGN e
    | id LPAREN e RPAREN
    | e DOT id LPAREN e RPAREN
    | LPAREN e RPAREN
    ;

