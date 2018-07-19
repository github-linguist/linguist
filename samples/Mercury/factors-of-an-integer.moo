:- module fac.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module float, int, list, math, string.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    list.filter_map(string.to_int, Args, CleanArgs),
    list.foldl((pred(Arg::in, !.IO::di, !:IO::uo) is det :-
                    factor(Arg, X),
                    io.format("factor(%d, [", [i(Arg)], !IO),
                    io.write_list(X, ",", io.write_int, !IO),
                    io.write_string("])\n", !IO)
               ), CleanArgs, !IO).

:- pred factor(int::in, list(int)::out) is det.
factor(N, Factors) :-
    Limit = float.truncate_to_int(math.sqrt(float(N))),
	factor(N, 2, Limit, [], Unsorted),
    list.sort_and_remove_dups([1, N | Unsorted], Factors).

:- pred factor(int, int, int, list(int), list(int)).
:- mode factor(in,  in,  in,  in,        out) is det.
factor(N, X, Limit, !Accumulator) :-
    ( if X  > Limit
          then true
          else ( if 0 = N mod X
                     then !:Accumulator = [X, N / X | !.Accumulator]
                     else true ),
               factor(N, X + 1, Limit, !Accumulator) ).

:- func factor(int) = list(int).
%:- mode factor(in) = out is det.
factor(N) = Factors :- factor(N, Factors).

:- end_module fac.
