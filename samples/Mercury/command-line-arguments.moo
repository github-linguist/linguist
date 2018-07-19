:- module cmd_line_args.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

main(!IO) :-
    io.progname("", ProgName, !IO),
    io.format("This program is named %s.\n", [s(ProgName)], !IO),
    io.command_line_arguments(Args, !IO),
    list.foldl2(print_arg, Args, 1, _, !IO).

:- pred print_arg(string::in, int::in, int::out, io::di, io::uo) is det.

print_arg(Arg, ArgNum, ArgNum + 1, !IO) :-
    io.format("the argument #%d is %s\n", [i(ArgNum), s(Arg)], !IO).
