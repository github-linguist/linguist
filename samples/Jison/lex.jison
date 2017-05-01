
// `%nonassoc` tells the parser compiler (JISON) that these tokens cannot occur more than once,
// i.e. input like '//a' (tokens '/', '/' and 'a') is not a legal input while '/a' (tokens '/' and 'a')
// *is* legal input for this grammar.
 
%nonassoc '/' '/!'

// Likewise for `%left`: this informs the LALR(1) grammar compiler (JISON) that these tokens
// *can* occur repeatedly, e.g. 'a?*' and even 'a**' are considered legal inputs given this
// grammar!
//
// Token `RANGE_REGEX` may seem the odd one out here but really isn't: given the `regex_base`
// choice/rule `regex_base range_regex`, which is recursive, this grammar tells JISON that 
// any input matching a sequence like `regex_base range_regex range_regex` *is* legal.
// If you do not want that to be legal, you MUST adjust the grammar rule set you match your
// actual intent.

%left '*' '+' '?' RANGE_REGEX


%% 

lex 
    : definitions include '%%' rules '%%' EOF
        {{ $$ = {macros: $1, rules: $4};
          if ($2) $$.actionInclude = $2;
          return $$; }}
    | definitions include '%%' rules EOF
        {{ $$ = {macros: $1, rules: $4};
          if ($2) $$.actionInclude = $2;
          return $$; }}
    ;

include
    : action
    |
    ;

definitions
    : definitions definition
        { $$ = $1; $$.concat($2); }
    | definition
        { $$ = [$1]; }
    ;

definition
    : name regex
        { $$ = [$1, $2]; }
    ;

name
    : NAME
        { $$ = yytext; }
    ;

rules
    : rules rule
        { $$ = $1; $$.push($2); }
    | rule
        { $$ = [$1]; }
    ;

rule
    : regex action
        { $$ = [$1, $2]; }
    ;

action
    : ACTION 
        { $$ = yytext; }
    ;

regex
    : start_caret regex_list end_dollar
        { $$ = $1+$2+$3; }
    ;

start_caret
    : '^'
        { $$ = '^'; }
    |
        { $$ = ''; }
    ;

end_dollar
    : '$'
        { $$ = '$'; }
    |
        { $$ = ''; }
    ;

regex_list
    : regex_list '|' regex_chain
        { $$ = $1+'|'+$3; }
    | regex_chain
    ;

regex_chain
    : regex_chain regex_base
        { $$ = $1+$2;}
    | regex_base
        { $$ = $1;}
    ;

regex_base
    : '(' regex_list ')'
        { $$ = '('+$2+')'; }
    | regex_base '+'
        { $$ = $1+'+'; }
    | regex_base '*'
        { $$ = $1+'*'; }
    | regex_base '?'
        { $$ = $1+'?'; }
    | '/' regex_base
        { $$ = '(?=' + $regex_base + ')'; }
    | '/!' regex_base
        { $$ = '(?!' + $regex_base + ')'; }
    | name_expansion
    | regex_base range_regex
        { $$ = $1+$2; }
    | any_group_regex
    | '.'
        { $$ = '.'; }
    | string
    ;

name_expansion
    : '{' name '}'
        {{ $$ = '{'+$2+'}'; }}
    ;

any_group_regex
    : ANY_GROUP_REGEX
        { $$ = yytext; }
    ;

range_regex
    : RANGE_REGEX
        { $$ = yytext; }
    ;

string
    : STRING_LIT
        { $$ = yy.prepareString(yytext.substr(1, yyleng-2)); }
    ;
