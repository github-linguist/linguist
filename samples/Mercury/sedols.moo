:- module sedol.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, require, string.

main(!IO) :-
    Input = [
        "710889",
        "B0YBKJ",
        "406566",
        "B0YBLH",
        "228276",
        "B0YBKL",
        "557910",
        "B0YBKR",
        "585284",
        "B0YBKT",
        "B00030"
     ],
     list.foldl(print_with_checksum, Input, !IO).

:- pred print_with_checksum(string::in, io::di, io::uo) is det.

print_with_checksum(S, !IO) :-
   io.format("%s%d\n", [s(S), i(sedol_checksum(S))], !IO).

:- func sedol_checksum(string) = int.

sedol_checksum(Sedol) = CheckSum :-
   Digits = string.foldr((func(C, A) = [to_sedol_code(C) | A]), Sedol, []),
   WeightedDigits = list.map_corresponding(int.times, Digits, [1, 3, 1, 7, 3, 9]),
   WeightedSum = list.foldl(int.plus, WeightedDigits, 0),
   CheckSum = (10 - (WeightedSum mod 10)) mod 10.

:- func to_sedol_code(char) = int.

to_sedol_code(Char) =
    ( if char.digit_to_int(Char, Code), not is_vowel(to_upper(Char))
    then Code
    else func_error("invalid SEDOL")
    ).

:- pred is_vowel(char::in) is semidet.

is_vowel('A').
is_vowel('E').
is_vowel('I').
is_vowel('O').
is_vowel('U').
