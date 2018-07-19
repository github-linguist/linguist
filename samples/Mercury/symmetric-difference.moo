:- module symdiff.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, set, string.

main(!IO) :-
    A = set(["John", "Bob", "Mary", "Serena"]),
    B = set(["Jim", "Mary", "John", "Bob"]),
    print_set("A\\B", DiffAB @ (A `difference` B), !IO),
    print_set("B\\A", DiffBA @ (B `difference` A), !IO),
    print_set("A symdiff B", DiffAB `union` DiffBA, !IO).

:- pred print_set(string::in, set(T)::in, io::di, io::uo) is det.

print_set(Desc, Set, !IO) :-
   to_sorted_list(Set, Elems),
   io.format("%11s: %s\n", [s(Desc), s(string(Elems))], !IO).
