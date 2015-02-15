:- module repeat.
:- interface.
:- import_module string, char, int.

:- func repeat_char(char, int) = string.
:- func repeat(string, int) = string.

:- implementation.
:- import_module stream, stream.string_writer, string.builder.

repeat_char(C, N) = string.duplicate_char(C, N).

repeat(String, Count) = Repeated :-
        S0 = string.builder.init,
        Repeated = string.builder.to_string(S),
        printn(string.builder.handle, Count, String, S0, S).

:- pred printn(Stream, int, string, State, State)
               <= (stream.writer(Stream, string, State),
                   stream.writer(Stream, character, State)).
:- mode printn(in, in, in, di, uo) is det.
printn(Stream, N, String, !S) :-
        ( N > 0 ->
                print(Stream, String, !S),
                printn(Stream, N - 1, String, !S)
        ; true ).
