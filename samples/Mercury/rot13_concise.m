% File: rot13_concise.m
% Main authors: Warwick Harvey <wharvey@cs.monash.edu.au>
%               Fergus Henderson <fjh@cs.mu.oz.au>
%
% rot13_concise:
%
% Program to read its input, apply the rot13 algorithm, and write it out
% again.
%
% This version is more concise (but less efficient) than its companion,
% rot13_verbose.
%
% Key features:
% - is independent of character set (e.g. ASCII, EBCDIC)
% - has proper error handling
%

:- module rot13_concise.

:- interface.
:- import_module io.

:- pred main(state, state).
:- mode main(di, uo) is det.

:- implementation.
:- import_module char, int, string.

% The length of `alphabet' should be a multiple of `cycle'.
alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".
cycle = 26.

rot_n(N, Char) = RotChar :-
	char_to_string(Char, CharString),
	( if sub_string_search(alphabet, CharString, Index) then
		NewIndex = (Index + N) mod cycle + cycle * (Index // cycle),
		index_det(alphabet, NewIndex, RotChar)
	else
		RotChar = Char
	).

rot13(Char) = rot_n(13, Char).

main -->
	read_char(Res),
	( { Res = ok(Char) },
		print(rot13(Char)),
		main
	; { Res = eof }
	; { Res = error(ErrorCode) },
		{ error_message(ErrorCode, ErrorMessage) },
		stderr_stream(StdErr),
		print(StdErr, "rot13: error reading input: "),
		print(StdErr, ErrorMessage),
		nl(StdErr)
	).


