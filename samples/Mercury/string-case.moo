:- module string_case.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, string.

main(!IO) :-
   S = "alphaBETA",
   io.format("uppercase       : %s\n", [s(to_upper(S))], !IO),
   io.format("lowercase       : %s\n", [s(to_lower(S))], !IO),
   io.format("capitalize first: %s\n", [s(capitalize_first(S))], !IO).
   % We can use uncaptitalize_first/1 to ensure the first character in a
   % string is lower-case.
