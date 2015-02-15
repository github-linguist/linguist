zig_zag(N) :-
	zig_zag(N, N).

% compute zig_zag for a matrix of Lig lines of Col columns
zig_zag(Lig, Col) :-
	length(M, Lig),
	maplist(init(Col), M),
	fill(M, 0, 0, 0, Lig, Col, up),
	% display the matrix
	maplist(print_line, M).


fill(M, Cur, L, C, NL, NC, _) :-
	L is NL - 1,
	C is NC - 1,
	nth0(L, M, Line),
	nth0(C, Line, Cur).

fill(M, Cur, L, C, NL, NC, Sens) :-
	nth0(L, M, Line),
	nth0(C, Line, Cur),
	Cur1 is Cur + 1,
	compute_next(NL, NC, L, C, Sens, L1, C1, Sens1),
	fill(M, Cur1, L1, C1, NL, NC, Sens1).


init(N, L) :-
	length(L, N).

% compute_next
% arg1 : Number of lnes of the matrix
% arg2 : number of columns of the matrix
% arg3 : current line
% arg4 : current column
% arg5 : current direction of movement
% arg6 : nect line
% arg7 : next column
% arg8 : next direction of movement
compute_next(_NL, NC, 0, Col, up, 0, Col1, down) :-
	Col < NC - 1,
	Col1 is Col+1.

compute_next(_NL, NC, 0, Col, up, 1, Col, down) :-
	Col is NC - 1.

compute_next(NL, _NC, Lig, 0, down, Lig1, 0, up) :-
	Lig < NL - 1,
	Lig1 is Lig+1.

compute_next(NL, _NC, Lig, 0, down, Lig, 1, up) :-
	Lig is NL - 1.

compute_next(NL, _NC, Lig, Col, down, Lig1, Col1, down) :-
	Lig < NL - 1,
	Lig1 is Lig + 1,
	Col1 is Col-1.

compute_next(NL, _NC, Lig, Col, down, Lig, Col1, up) :-
	Lig is NL - 1,
	Col1 is Col+1.

compute_next(_NL, NC, Lig, Col, up, Lig1, Col1, up) :-
	Col < NC - 1,
	Lig1 is Lig - 1,
	Col1 is Col+1.

compute_next(_NL, NC, Lig, Col, up, Lig1, Col, down) :-
	Col is NC - 1,
	Lig1 is Lig + 1.




print_line(L) :-
	maplist(print_val, L),
	nl.

print_val(V) :-
	writef('%3r ', [V]).
