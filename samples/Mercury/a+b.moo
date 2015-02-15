:- module a_plus_b.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

main(!IO) :-
   io.read_line_as_string(Result, !IO),
   ( if
        Result = ok(Line),
        [AStr, BStr] = string.words(Line),
        string.to_int(AStr, A), string.to_int(BStr, B)
     then
        io.format("%d\n", [i(A + B)], !IO)
     else
        true
    ).
