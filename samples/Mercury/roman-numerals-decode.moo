:- module test_roman.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module string.

:- type conversion_error --->  not_a_roman_number.

:- func build_int(list(char), int, int) = int.
:- func from_roman(string) = int.
:- pred roman_to_int(char::in, int::out) is semidet.

from_roman(Roman) = Decimal :-
  List = reverse(to_char_list(Roman)),
  Decimal = build_int(List, 0, 0).

build_int([], LastValue, Accumulator) = LastValue + Accumulator.
build_int([Digit|Rest], LastValue, Accumulator) = Sum :-
  ( roman_to_int(Digit, Value) ->
      ( Value < LastValue ->
          Sum = build_int(Rest, Value, Accumulator - LastValue)
      ;   Sum = build_int(Rest, Value, Accumulator + LastValue) )
  ;   throw(not_a_roman_number) ).

roman_to_int('I', 1).
roman_to_int('V', 5).
roman_to_int('X', 10).
roman_to_int('L', 50).
roman_to_int('C', 100).
roman_to_int('D', 500).
roman_to_int('M', 1000).

main(!IO) :-
    command_line_arguments(Args, !IO),
    foldl((pred(Arg::in, !.IO::di, !:IO::uo) is det :-
              format("%s => %d\n", [s(Arg), i(from_roman(Arg))], !IO)),
          Args, !IO).

:- end_module test_roman.
