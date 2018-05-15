/* File taken from http://home.adelphi.edu/~sbloch/class/271/examples/lexyacc/words2.y . */
%{
#include <stdio.h>

int at_end = 0;
extern int yychar;
%}

%token ARTICLE VERB NOUN ADJECTIVE ADVERB PREPOSITION END
%start sentence

%%
sentence:	nphrase VERB termpunct		{ printf ("Simple noun-verb sentence.\n");
						YYACCEPT;}
	|	nphrase VERB vmodifier termpunct	{ printf ("Sentence with modified verb\n");
						YYACCEPT;}
	|	nphrase VERB nphrase termpunct	{ printf ("Sentence with object\n");
						YYACCEPT;}
	|	END				{ printf ("Got EOF from lex.\n");
						YYACCEPT;}
			/* All these YYACCEPTS are needed so yyparse will return immediately,
			rather than waiting for the first token of the next sentence.  They
			wouldn't be necessary if the main program were only calling yyparse() once.  */
	;
termpunct:	'.'				;
	|	'!'				;
	;
nphrase:	modifiednoun			;
	|	ARTICLE modifiednoun		{ printf ("\tGot an article\n"); }
	;
modifiednoun:	NOUN
	|	nmodifier modifiednoun		{ printf ("\tmodified noun\n"); }
	;
nmodifier:	ADJECTIVE			;
	|	ADVERB nmodifier		{ printf ("\tadded an adverb to a noun modifier\n"); }
	;
vmodifier:	ADVERB				;
	|	ADVERB vmodifier		{ printf ("\tadded an adverb to a verb modifier\n"); }
	|	PREPOSITION nphrase		{ printf ("\tprepositional phrase\n"); }
	;

%%
int main (void) {
	while (! at_end) {
		yyparse();
		}
	printf ("Wasn't that fun?\n");
	}

/* Added because panther doesn't have liby.a installed. */
int yyerror (char *msg) {
	return fprintf (stderr, "YACC: %s; yychar=%d\n", msg, yychar);
	}