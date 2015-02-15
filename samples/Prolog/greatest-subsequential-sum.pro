:- use_module(library(chr)).

:- chr_constraint
	init_chr/2,
	seq/2,
	% gss(Deb, Len, TT)
	gss/3,
	% gsscur(Deb, Len, TT, IdCur)
	gsscur/4,
	memoseq/3,
	clean/0,
	greatest_subsequence/0.


greatest_subsequence <=>
	L = [-1 , -2 , 3 , 5 , 6 , -2 , -1 , 4 , -4 , 2 , -1],
	init_chr(1, L),
	find_chr_constraint(gss(Deb, Len, V)),
	clean,
	writeln(L),
	forall(between(1, Len, I),
	       (   J is I+Deb-1, nth1(J, L, N), format('~w ', [N]))),
	format('==> ~w ~n', [V]).

% destroy last constraint gss
clean \ gss(_,_,_) <=> true.
clean <=> true.

init_chr_end @ init_chr(_, []) <=> gss(0, 0, 0), gsscur(1,0,0,1).

init_chr_loop @ init_chr(N, [H|T]) <=> seq(N, H), N1 is N+1, init_chr(N1, T).

% here, we memorize the list
gsscur_with_negative @ gsscur(Deb, Len, TT, N),  seq(N, V) <=> V =< 0 |
             memoseq(Deb, Len, TT),
	     TT1 is TT + V,
	     N1 is N+1,
	     % if TT1 becomes negative,
	     % we begin a new subsequence
	     (	 TT1 < 0 -> gsscur(N1,0,0,N1)
	     ;	 Len1 is Len + 1, gsscur(Deb, Len1, TT1, N1)).

gsscur_with_positive @ gsscur(Deb, Len, TT, N),  seq(N, V) <=> V > 0 |
             TT1 is TT + V,
	     N1 is N+1,
	     Len1 is Len + 1,
	     gsscur(Deb, Len1, TT1, N1).

gsscur_end @ gsscur(Deb, Len, TT, _N) <=> memoseq(Deb, Len, TT).

memoseq(_DC, _LC, TTC), gss(D, L, TT) <=> TTC =< TT |
	       gss(D, L, TT).

memoseq(DC, LC, TTC), gss(_D, _L, TT) <=> TTC > TT |
		gss(DC, LC, TTC).
