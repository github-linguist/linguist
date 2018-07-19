:- dynamic pt/6.
voronoi :-
	V is random(20) + 20,
	retractall(pt(_,_,_,_)),
	forall(between(1, V, I),
	       ( X is random(390) + 5,
	         Y is random(390) + 5,
		 R is random(65535),
		 G is random(65535),
		 B is random(65535),
		 assertz(pt(I,X,Y, R, G, B))
	       )),
	voronoi(manhattan, V),
	voronoi(euclide, V),
	voronoi(minkowski_3, V).

voronoi(Distance, V) :-
	sformat(A, 'Voronoi 400X400 ~w ~w', [V, Distance]),
	new(D, window(A)),
	send(D, size, size(400,400)),
	new(Img, image(@nil, width := 400, height := 400 , kind := pixmap)),

        % get the list of the sites
	bagof((N, X, Y), R^G^B^pt(N, X, Y, R, G, B), L),

	forall(between(0,399, I),
	       forall(between(0,399, J),
		   (  get_nearest_site(V, Distance, I, J, L, S),
		      pt(S, _, _, R, G, B),
		      send(Img, pixel(I, J, colour(@default, R, G, B)))))),

	new(Bmp, bitmap(Img)),
	send(D, display, Bmp, point(0,0)),
	send(D, open).

% define predicatea foldl (functionnal spirit)
foldl([], _Pred, R, R).

foldl([H | T], Pred, Acc, R) :-
	call(Pred, H, Acc, R1),
	foldl(T, Pred, R1, R).

% predicate for foldl
compare(Distance, XP, YP, (N, X, Y), (D, S), R) :-
	call(Distance, XP, YP, X, Y, DT),
	(   DT < D -> R = (DT, N) ; R = (D, S)).

% use of a fake site for the init of foldl
get_nearest_site(Distance, I, J, L, S) :-
	foldl(L, compare(Distance, I, J),  (65535, nil), (_, S)).



manhattan(X1, Y1, X2, Y2, D) :-
	D is abs(X2 - X1) + abs(Y2-Y1).

euclide(X1, Y1, X2, Y2, D) :-
	D is sqrt((X2 - X1)**2 + (Y2-Y1)**2).

minkowski_3(X1, Y1, X2, Y2, D) :-
	D is (abs(X2 - X1)**3 + abs(Y2-Y1)**3)**0.33.
