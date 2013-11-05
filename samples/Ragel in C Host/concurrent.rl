/*
 * Show off concurrent abilities.
 */

#include <iostream>
#include <stdlib.h>
#include <stdio.h>

using namespace std;

#define BUFSIZE 2048

struct Concurrent
{
	int cur_char;
	int start_word;
	int start_comment;
	int start_literal;

	int cs;

	int init( );
	int execute( const char *data, int len, bool isEof );
	int finish( );
};

%%{
	machine Concurrent;

	action next_char {
		cur_char += 1;
	}

	action start_word {
		start_word = cur_char;
	}
	action end_word {
		cout << "word: " << start_word << 
				" " << cur_char-1 << endl;
	}

	action start_comment {
		start_comment = cur_char;
	}
	action end_comment {
		cout << "comment: " << start_comment <<
				" " << cur_char-1 << endl;
	}

	action start_literal {
		start_literal = cur_char;
	}
	action end_literal {
		cout << "literal: " << start_literal <<
				" " << cur_char-1 << endl;
	}

	# Count characters.
	chars = ( any @next_char )*;

	# Words are non-whitespace. 
	word = ( any-space )+ >start_word %end_word;
	words = ( ( word | space ) $1 %0 )*;

	# Finds C style comments. 
	comment = ( '/*' any* :>> '*/' ) >start_comment %end_comment;
	comments = ( comment | any )**;

	# Finds single quoted strings. 
	literalChar = ( any - ['\\] ) | ( '\\' . any );
	literal = ('\'' literalChar* '\'' ) >start_literal %end_literal;
	literals = ( ( literal | (any-'\'') ) $1 %0 )*;

	main := chars | words | comments | literals;
}%%

%% write data;

int Concurrent::init( )
{
	%% write init;
	cur_char = 0;
	return 1;
}

int Concurrent::execute( const char *data, int len, bool isEof )
{
	const char *p = data;
	const char *pe = data + len;
	const char *eof = isEof ? pe : 0;

	%% write exec;

	if ( cs == Concurrent_error )
		return -1;
	if ( cs >= Concurrent_first_final )
		return 1;
	return 0;
}

int Concurrent::finish( )
{
	if ( cs == Concurrent_error )
		return -1;
	if ( cs >= Concurrent_first_final )
		return 1;
	return 0;
}

Concurrent concurrent;
char buf[BUFSIZE];

int main()
{
	concurrent.init();
	while ( 1 ) {
		int len = fread( buf, 1, BUFSIZE, stdin );
		concurrent.execute( buf, len, len != BUFSIZE );
		if ( len != BUFSIZE )
			break;
	}

	if ( concurrent.finish() <= 0 )
		cerr << "concurrent: error parsing input" << endl;
	return 0;
}
