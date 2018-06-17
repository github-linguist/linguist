%{
/*
 * Test program: Advanced Calculator
 * by Zhao Cheng 5/20/2012
 */
%}

%union {
    double val;    /* For returning numbers.  */
    symrec *tptr;  /* For returning symbol-table pointers.  */
}

%token <val> NUMBER
%token <tptr> VAR FNCT

%right '='
%left '+' '-'
%left '*' '/'
%right '^'
%left NEG

%type <val> expression

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "calc.h"  /* Contains definition of `symrec'.  */
%}

%%

statement
    : /* empty */ { exit(0); }
    | expression { printf("= %f\n", $1); }
    ;

expression
    : NUMBER { $$ = $1; }
    | VAR    { $$ = $1->value.var; }
    | VAR '=' expression        { $$ = $3; $1->value.var = $3; }
    | FNCT '(' expression ')'   { $$ = (*($1->value.fnctptr))($3); }
    | expression '*' expression { $$ = $1 * $3; }
    | expression '/' expression { $$ = $1 / $3; }
    | expression '+' expression { $$ = $1 + $3; }
    | expression '-' expression { $$ = $1 - $3; }
    | expression '^' expression { $$ = pow($1, $3); }
    | '-' expression %prec NEG  { $$ = -$2; }
    | '(' expression ')'        { $$ = $2; }
    ;

%%
struct init
{
    char const *fname;
    double (*fnct) (double);
};
struct init const arith_fncts[] =
{
    "sin"   , sin   , 
    "asin"  , asin  , 
    "cos"   , cos   , 
    "acos"  , acos  , 
    "tan"   , tan   , 
    "atan"  , atan  , 
    "ceil"  , ceil  , 
    "floor" , floor , 
    "abs"   , fabs  , 
    "ln"    , log   , 
    "log"   , log10 , 
    "lg"    , log2  , 
    "exp"   , exp   , 
    "sqrt"  , sqrt  , 
    0       , 0
};
/* The symbol table: a chain of `struct symrec'.  */
symrec *sym_table;
/* Put arithmetic functions in table.  */
void init_table (void)
{
    int i;
    symrec *ptr;
    for (i = 0; arith_fncts[i].fname != 0; i++) {
        ptr = putsym (arith_fncts[i].fname, FNCT);
        ptr->value.fnctptr = arith_fncts[i].fnct;
    }
}
int main()
{
    init_table();
    while (yyparse() == 0)
        ;
    return 0;
}
void yyerror(const char *msg)
{
    fprintf(stderr, "Error: %s\n", msg);
}
symrec *
putsym (char const *sym_name, int sym_type)
{
  symrec *ptr;
  ptr = (symrec *) malloc (sizeof (symrec));
  ptr->name = (char *) malloc (strlen (sym_name) + 1);
  strcpy (ptr->name,sym_name);
  ptr->type = sym_type;
  ptr->value.var = 0; /* Set value to 0 even if fctn.  */
  ptr->next = (struct symrec *)sym_table;
  sym_table = ptr;
  return ptr;
}
symrec *
getsym (char const *sym_name)
{
  symrec *ptr;
  for (ptr = sym_table; ptr != (symrec *) 0;
       ptr = (symrec *)ptr->next)
    if (strcmp (ptr->name,sym_name) == 0)
      return ptr;
  return 0;
}
