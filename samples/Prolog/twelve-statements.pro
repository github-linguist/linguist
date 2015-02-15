puzzle :-
        % 1. This is a numbered list of twelve statements.
	L = [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12],
	L ins 0..1,
	element(1, L, 1),

       % 2.  Exactly 3 of the last 6 statements are true.
	A2 #<==>  A7 + A8 + A9 + A10 + A11 + A12 #= 3,

       % 3.  Exactly 2 of the even-numbered statements are true.
	A3 #<==> A2 + A4 + A6 + A8 + A10 + A12 #= 2,

       % 4.  If statement 5 is true, then statements 6 and 7 are both true.
	A4 #<==> (A5 #==> (A6 #/\ A7)),

       % 5.  The 3 preceding statements are all false.
	A5 #<==> A2 + A3 + A4 #= 0,

       % 6.  Exactly 4 of the odd-numbered statements are true.
	A6 #==> A1 + A3 + A5 + A7 + A9 + A11 #= 4,

        % 7.  Either statement 2 or 3 is true, but not both.
	A7 #<==> A2 + A3 #= 1,

        % 8.  If statement 7 is true, then 5 and 6 are both true.
	A8 #<==> (A7 #==>  A5 #/\ A6),


        % 9.  Exactly 3 of the first 6 statements are true.
	A9 #<==> A1 + A2 + A3 + A4 + A5 + A6 #= 3,

        % 10.  The next two statements are both true.
	A10 #<==> A11 #/\ A12,

        % 11.  Exactly 1 of statements 7, 8 and 9 are true.
	A11 #<==> A7 + A8 + A9 #= 1,

        % 12.  Exactly 4 of the preceding statements are true.
	A12 #<==> A1 + A2 + A3 + A4 + A5 + A6 + A7 +A8 + A9 + A10 + A11 #= 4,

	label(L),
        numlist(1, 12, NL),
	write('Statements '),
	maplist(my_write, NL, L),
	writeln('are true').


my_write(N, 1) :-
	format('~w ', [N]).

my_write(_N, 0).
