:- module guess.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module random, string.

main(!IO) :-
        time(Time, !IO),
        random.init(Time, Rand),
        random.random(1, 10, N, Rand, _),
        main(from_int(N) ++ "\n", !IO).

:- pred main(string::in, io::di, io::uo) is det.
main(N, !IO) :-
        io.write_string("Guess the number: ", !IO),
        io.read_line_as_string(Res, !IO),
        (
                Res = ok(S),
                ( if S = N then io.write_string("Well guessed!\n", !IO)
                  else main(N, !IO) )
        ;
                Res = error(E)
        ;
                Res = eof
        ).

:- pred time(int::out, io::di, io::uo) is det.

:- pragma foreign_decl("C", "#include <time.h>").
:- pragma foreign_proc("C", time(Int::out, _IO0::di, _IO1::uo),
                       [will_not_call_mercury, promise_pure],
                       "Int = time(NULL);").
