sleep_sort(L) :-
	thread_pool_create(rosetta, 1024, []) ,
	maplist(initsort, L, LID),
	maplist(thread_join, LID, _LStatus),
	thread_pool_destroy(rosetta).

initsort(V, Id) :-
	thread_create_in_pool(rosetta, (sleep(V), writeln(V)), Id, []).
