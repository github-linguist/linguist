% Lexer
 numeric(X) :- 48 =< X, X =< 57.
 not_numeric(X) :- 48 > X ; X > 57.

 lex1([], []).
 lex1([40|Xs], ['('|Ys]) :- lex1(Xs, Ys).
 lex1([41|Xs], [')'|Ys]) :- lex1(Xs, Ys).
 lex1([43|Xs], ['+'|Ys]) :- lex1(Xs, Ys).
 lex1([45|Xs], ['-'|Ys]) :- lex1(Xs, Ys).
 lex1([42|Xs], ['*'|Ys]) :- lex1(Xs, Ys).
 lex1([47|Xs], ['/'|Ys]) :- lex1(Xs, Ys).
 lex1([X|Xs], [N|Ys]) :- numeric(X), N is X - 48, lex1(Xs, Ys).

 lex2([], []).
 lex2([X], [X]).
 lex2([Xa,Xb|Xs], [Xa|Ys]) :- atom(Xa), lex2([Xb|Xs], Ys).
 lex2([Xa,Xb|Xs], [Xa|Ys]) :- number(Xa), atom(Xb), lex2([Xb|Xs], Ys).
 lex2([Xa,Xb|Xs], [Y|Ys]) :- number(Xa), number(Xb), N is Xa * 10 + Xb, lex2([N|Xs], [Y|Ys]).

 % Parser
 oper(1, *, X, Y, X * Y). oper(1, /, X, Y, X / Y).
 oper(2, +, X, Y, X + Y). oper(2, -, X, Y, X - Y).

 num(D) --> [D], {number(D)}.

 expr(0, Z) --> num(Z).
 expr(0, Z) --> {Z = (X)}, ['('], expr(2, X), [')'].

 expr(N, Z) --> {succ(N0, N)}, {oper(N, Op, X, Y, Z)}, expr(N0, X), [Op], expr(N, Y).
 expr(N, Z) --> {succ(N0, N)}, expr(N0, Z).

 parse(Tokens, Expr) :- expr(2, Expr, Tokens, []).


 % Evaluator
 evaluate(E, E) :- number(E).
 evaluate(A + B, E) :- evaluate(A, Ae), evaluate(B, Be), E is Ae + Be.
 evaluate(A - B, E) :- evaluate(A, Ae), evaluate(B, Be), E is Ae - Be.
 evaluate(A * B, E) :- evaluate(A, Ae), evaluate(B, Be), E is Ae * Be.
 evaluate(A / B, E) :- evaluate(A, Ae), evaluate(B, Be), E is Ae / Be.

 % Solution
 calculator(String, Value) :-
    lex1(String, Tokens1),
    lex2(Tokens1, Tokens2),
    parse(Tokens2, Expression),
    evaluate(Expression, Value).

 % Example use
 % calculator("(3+50)*7-9", X).
