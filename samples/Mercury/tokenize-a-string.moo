:- module string_tokenize.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, string.

main(!IO) :-
    Tokens = string.split_at_char((','), "Hello,How,Are,You,Today"),
    io.write_list(Tokens, ".", io.write_string, !IO),
    io.nl(!IO).
