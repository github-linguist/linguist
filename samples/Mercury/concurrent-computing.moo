:- module concurrent_computing.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module thread.

main(!IO) :-
   spawn(io.print_cc("Enjoy\n"), !IO),
   spawn(io.print_cc("Rosetta\n"), !IO),
   spawn(io.print_cc("Code\n"), !IO).
