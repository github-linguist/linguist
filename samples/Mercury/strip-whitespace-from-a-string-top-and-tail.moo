:- module top_and_tail.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, string.

main(!IO) :-
   TestPhrase = "\t\r\n String with spaces \t\r\n ",
   io.format("leading ws removed: %s\n", [s(lstrip(TestPhrase))], !IO),
   io.format("trailing ws removed: %s\n", [s(rstrip(TestPhrase))], !IO),
   io.format("both removed: %s\b", [s(strip(TestPhrase))], !IO).
