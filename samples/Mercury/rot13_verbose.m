% File: rot13_verbose.m
% Main author: Warwick Harvey <wharvey@cs.monash.edu.au>
% Additional input: Fergus Henderson <fjh@cs.mu.oz.au>

%
% rot13_verbose:
%
% Program to read its input, apply the rot13 algorithm, and write it out
% again.
%
% This version is more verbose (and more efficient) than its companion,
% rot13_concise.
%
% Key features:
% - is independent of character set (e.g. ASCII, EBCDIC)
% - has proper error handling
% - reasonably efficient (uses a table to do the rotation)
%

:- module rot13_verbose.

:- interface.
:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.
:- import_module char, int, require.

	% rot13a/2
	% A table to map the alphabetic characters to their rot13 equivalents
	% (fails if the input is not alphabetic).
:- pred rot13a(char, char).
:- mode rot13a(in, out) is semidet.

rot13a('a', 'n').
rot13a('b', 'o').
rot13a('c', 'p').
rot13a('d', 'q').
rot13a('e', 'r').
rot13a('f', 's').
rot13a('g', 't').
rot13a('h', 'u').
rot13a('i', 'v').
rot13a('j', 'w').
rot13a('k', 'x').
rot13a('l', 'y').
rot13a('m', 'z').
rot13a('n', 'a').
rot13a('o', 'b').
rot13a('p', 'c').
rot13a('q', 'd').
rot13a('r', 'e').
rot13a('s', 'f').
rot13a('t', 'g').
rot13a('u', 'h').
rot13a('v', 'i').
rot13a('w', 'j').
rot13a('x', 'k').
rot13a('y', 'l').
rot13a('z', 'm').
rot13a('A', 'N').
rot13a('B', 'O').
rot13a('C', 'P').
rot13a('D', 'Q').
rot13a('E', 'R').
rot13a('F', 'S').
rot13a('G', 'T').
rot13a('H', 'U').
rot13a('I', 'V').
rot13a('J', 'W').
rot13a('K', 'X').
rot13a('L', 'Y').
rot13a('M', 'Z').
rot13a('N', 'A').
rot13a('O', 'B').
rot13a('P', 'C').
rot13a('Q', 'D').
rot13a('R', 'E').
rot13a('S', 'F').
rot13a('T', 'G').
rot13a('U', 'H').
rot13a('V', 'I').
rot13a('W', 'J').
rot13a('X', 'K').
rot13a('Y', 'L').
rot13a('Z', 'M').

	% rot13/2
	% Applies the rot13 algorithm to a character.
:- pred rot13(char, char).
:- mode rot13(in, out) is det.

rot13(Char, RotChar) :-
	( if rot13a(Char, TmpChar) then
		RotChar = TmpChar
	else
		RotChar = Char
	).

main -->
	io__read_char(Res),
	( { Res = ok(Char) },
		{ rot13(Char, RotChar) },
		io__write_char(RotChar),
		main
	; { Res = eof }
	; { Res = error(ErrorCode) },
		{ io__error_message(ErrorCode, ErrorMessage) },
		io__stderr_stream(StdErr),
		io__write_string(StdErr, "rot13: error reading input: "),
		io__write_string(StdErr, ErrorMessage),
		io__nl(StdErr)
	).

