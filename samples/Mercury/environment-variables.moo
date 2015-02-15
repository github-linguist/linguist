:- module env_var.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module maybe, string.

main(!IO) :-
    io.get_environment_var("HOME", MaybeValue, !IO),
    (
        MaybeValue = yes(Value),
        io.write_string("HOME is " ++ Value ++ "\n", !IO)
    ;
        MaybeValue = no,
        io.write_string("environment variable HOME not set\n", !IO)
    ).
