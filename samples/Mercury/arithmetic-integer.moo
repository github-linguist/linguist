:- module arith_int.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if
        Args = [AStr, BStr],
        string.to_int(AStr, A),
        string.to_int(BStr, B)
      then
        io.format("A + B = %d\n", [i(A + B)], !IO),
        io.format("A - B = %d\n", [i(A - B)], !IO),
        io.format("A * B = %d\n", [i(A * B)], !IO),

        % Division: round towards zero.
        %
        io.format("A / B = %d\n", [i(A / B)], !IO),

        % Division: round towards minus infinity.
        %
        io.format("A div B = %d\n", [i(A div B)], !IO),

        % Modulus: X mod Y = X - (X div Y) * Y.
        %
        io.format("A mod B = %d\n", [i(A mod B)], !IO),

        % Remainder: X rem Y = X - (X / Y) * Y.
        %
        io.format("A rem B = %d\n", [i(A rem B)], !IO),

        % Exponentiation is done using the function int.pow/2.
        %
        io.format("A `pow` B = %d\n", [i(A `pow` B)], !IO)
      else
        io.set_exit_status(1, !IO)
    ).
