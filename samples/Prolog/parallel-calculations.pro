threaded_decomp(Number,ID):-
	thread_create(
		      (prime_decomp(Number,Y),
		       thread_exit((Number,Y)))
		     ,ID,[]).

threaded_decomp_list(List,Erg):-
	maplist(threaded_decomp,List,IDs),
	maplist(thread_join,IDs,Results),
	maplist(pack_exit_out,Results,Smallest_Factors_List),
	largest_min_factor(Smallest_Factors_List,Erg).

pack_exit_out(exited(X),X).
%Note that here some error handling should happen.

largest_min_factor([(N,Facs)|A],(N2,Fs2)):-
	min_list(Facs,MF),
	largest_min_factor(A,(N,MF,Facs),(N2,_,Fs2)).

largest_min_factor([],Acc,Acc).
largest_min_factor([(N1,Facs1)|Rest],(N2,MF2,Facs2),Goal):-
	min_list(Facs1, MF1),
	(MF1 > MF2->
	largest_min_factor(Rest,(N1,MF1,Facs1),Goal);
	largest_min_factor(Rest,(N2,MF2,Facs2),Goal)).


format_it(List):-
	threaded_decomp_list(List,(Number,Factors)),
	format('Number with largest minimal Factor is ~w\nFactors are ~w\n',
	       [Number,Factors]).
