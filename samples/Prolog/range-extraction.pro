range_extract :-
	L = [0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
	     15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
	     25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
	     37, 38, 39] ,
	writeln(L),
	pack_Range(L, LP),
	maplist(study_Range, R, LP),
	extract_Range(LA, R),
	atom_chars(A, LA),
	writeln(A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% extract_Range(?In, ?Out)
% In  : '-6,-3--1,3-5,7-11,14,15,17-20' =>
% Out : [-6], [-3--1], [3-5],[7-11], [14],[15], [17-20]
%
extract_Range([], []).


extract_Range(X , [Range | Y1]) :-
	get_Range(X, U-U, Range, X1),
	extract_Range(X1, Y1).



get_Range([], Range-[], Range, []).
get_Range([','|B], Range-[], Range, B) :- !.

get_Range([A | B], EC, Range, R) :-
	append_dl(EC, [A | U]-U, NEC),
	get_Range(B, NEC, Range, R).


append_dl(X-Y, Y-Z, X-Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% study Range(?In, ?Out)
% In  : [-6]
% Out : [-6,-6]
%
% In  : [-3--1]
% Out : [-3, -1]
%
study_Range(Range1, [Deb, Deb]) :-
       catch(number_chars(Deb, Range1), Deb, false).

study_Range(Range1, [Deb, Fin]) :-
       append(A, ['-'|B], Range1),
       A \= [],
       number_chars(Deb, A),
       number_chars(Fin, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
:- use_module(library(clpfd)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Pack Range(?In, ?Out)
% In  : -6,
% Out : [-6]
%
% In  : -3, -2,-1
% Out : [-3,-1]
%
pack_Range([],[]).

pack_Range([X|Rest],[[X | V]|Packed]):-
    run(X,Rest, [X|V], RRest),
    pack_Range(RRest,Packed).



run(Fin,[Other|RRest], [Deb, Fin],[Other|RRest]):-
	Fin #\= Deb,
	Fin #\= Deb + 1,
	Other #\= Fin+1.

run(Fin,[],[_Var, Fin],[]).

run(Var,[Var1|LRest],[Deb, Fin], RRest):-
	Fin #\= Deb,
	Fin #\= Deb + 1,
	Var1 #= Var + 1,
	run(Var1,LRest,[Deb, Fin], RRest).

run(Val,[Other|RRest], [Val, Val],[Other|RRest]).
