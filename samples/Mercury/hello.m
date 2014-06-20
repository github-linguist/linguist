% "Hello World" in Mercury.

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module hello.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
	io.write_string("Hello, world\n", !IO).
