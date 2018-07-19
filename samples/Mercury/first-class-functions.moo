:- module firstclass.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module exception, list, math, std_util.

main(!IO) :-
    Forward = [sin,  cos,  (func(X) = ln(X))],
    Reverse = [asin, acos, (func(X) = exp(X))],
    Results = map_corresponding(
        (func(F, R) = compose(R, F, 0.5)),
        Forward, Reverse),
    write_list(Results, ", ", write_float, !IO),
    write_string("\n", !IO).
