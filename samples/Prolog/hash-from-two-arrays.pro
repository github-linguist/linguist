% this one with side effect hash table creation

:-dynamic hash/2.

make_hash([],[]).
make_hash([H|Q],[H1|Q1]):-
	assert(hash(H,H1)),
	make_hash(Q,Q1).

:-make_hash([un,deux,trois],[[a,b,c],[d,e,f],[g,h,i]])


% this one without side effects

make_hash_pure([],[],[]).
make_hash_pure([H|Q],[H1|Q1],[hash(H,H1)|R]):-
	make_hash_pure(Q,Q1,R).

:-make_hash_pure([un,deux,trois],[[a,b,c],[d,e,f],[g,h,i]],L),findall(M,(member(M,L),assert(M)),L).
