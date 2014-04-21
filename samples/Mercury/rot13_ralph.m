% ---------------------------------------------------------------------------- %
% rot13_ralph.m
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
% Tue Jan  9 18:10:44 GMT 2001
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%
% Short and sweet.
%
% ---------------------------------------------------------------------------- %

:- module rot13_ralph.

:- interface.

:- import_module io.



:- pred main(io__state::di, io__state::uo) is det.

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %

:- implementation.

:- import_module int, require.

% ---------------------------------------------------------------------------- %

main -->
    io__read_byte(Result),
    (   { Result = ok(X) },         io__write_byte(rot13(X)), main
    ;   { Result = eof }
    ;   { Result = error(ErrNo)},   { error(io__error_message(ErrNo)) }
    ).

% ---------------------------------------------------------------------------- %

:- func rot13(int) = int.

rot13(X) =
    (      if 0'a =< X, X =< 0'z then Rot13(X, 0'a)
      else if 0'A =< X, X =< 0'Z then Rot13(X, 0'A)
      else X
    )
 :-
    Rot13 = ( func(C, A) = ((13 + C - A) `rem` 26) + A ).

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %
