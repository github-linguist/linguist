/*
 * Perform the basic line parsing of input performed by awk.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

%%{
	machine awkemu;

	action start_word {
		ws[nwords] = fpc;
	}

	action end_word {
		we[nwords++] = fpc;
	}

	action start_line {
		nwords = 0;
		ls = fpc;
	}

	action end_line {
		printf("endline(%i): ", nwords );
		fwrite( ls, 1, p - ls, stdout );
		printf("\n");

		for ( i = 0; i < nwords; i++ ) {
			printf("  word: ");
			fwrite( ws[i], 1, we[i] - ws[i], stdout );
			printf("\n");
		}
	}

	# Words in a line.
	word = ^[ \t\n]+;

	# The whitespace separating words in a line.
	whitespace = [ \t];

	# The components in a line to break up. Either a word or a single char of
	# whitespace. On the word capture characters.
	blineElements = word >start_word %end_word | whitespace;

	# Star the break line elements. Just be careful to decrement the leaving
	# priority as we don't want multiple character identifiers to be treated as
	# multiple single char identifiers.
	line = ( blineElements** '\n' ) >start_line @end_line;

	# Any number of lines.
	main := line*;
}%%

%% write data noerror nofinal;

#define MAXWORDS 256
#define BUFSIZE 4096
char buf[BUFSIZE];

int main()
{
	int i, nwords = 0;
	char *ls = 0;
	char *ws[MAXWORDS];
	char *we[MAXWORDS];

	int cs;
	int have = 0;

	%% write init;

	while ( 1 ) {
		char *p, *pe, *data = buf + have;
		int len, space = BUFSIZE - have;
		/* fprintf( stderr, "space: %i\n", space ); */

		if ( space == 0 ) { 
			fprintf(stderr, "buffer out of space\n");
			exit(1);
		}

		len = fread( data, 1, space, stdin );
		/* fprintf( stderr, "len: %i\n", len ); */
		if ( len == 0 )
			break;

		/* Find the last newline by searching backwards. This is where 
		 * we will stop processing on this iteration. */
		p = buf;
		pe = buf + have + len - 1;
		while ( *pe != '\n' && pe >= buf )
			pe--;
		pe += 1;

		/* fprintf( stderr, "running on: %i\n", pe - p ); */

		%% write exec;

		/* How much is still in the buffer. */
		have = data + len - pe;
		if ( have > 0 )
			memmove( buf, pe, have );

		/* fprintf(stderr, "have: %i\n", have ); */

		if ( len < space )
			break;
	}

	if ( have > 0 )
		fprintf(stderr, "input not newline terminated\n");
	return 0;
}