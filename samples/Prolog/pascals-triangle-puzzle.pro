:- use_module(library(clpfd)).

puzzle(Ts, X, Y, Z) :-
    Ts =   [ [151],
            [_, _],
          [40, _, _],
         [_, _, _, _],
       [X, 11, Y, 4, Z]],
    Y #= X + Z, triangle(Ts), append(Ts, Vs), Vs ins 0..sup, label(Vs).

triangle([T|Ts]) :- ( Ts = [N|_] -> triangle_(T, N), triangle(Ts) ; true ).

triangle_([], _).
triangle_([T|Ts], [A,B|Rest]) :- T #= A + B, triangle_(Ts, [B|Rest]).

% ?- puzzle(_,X,Y,Z).
% X = 5,
% Y = 13,
% Z = 8 ;
