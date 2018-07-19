%___________________________________________________________________________
% Bubble sort

bubble(0, Res, Res, sorted).
bubble(Len, [A,B|T], Res, unsorted) :- A > B, !, bubble(Len,[B,A|T], Res, _).
bubble(Len, [A|T], [A|Ts], Ch) :- L is Len-1, bubble(L, T, Ts, Ch).

bubblesort(In, Out) :- length(In, Len), bubblesort(Len, In, Out).
bubblesort(0, In, In).
bubblesort(Len, In, Out) :-
	bubble(Len, In, Bubbled, SortFlag),  % bubble the list
	(SortFlag=sorted -> Out=Bubbled;     % list is already sorted
	 SegLen is Len - 1,		     % one fewer to process
	 writef('bubbled=%w\n', [Bubbled]),  % show progress
	 bubblesort(SegLen, Bubbled, Out)).

test :-  In = [8,9,1,3,4,2,6,5,4],
	 writef('  input=%w\n', [In]),
	 bubblesort(In, R),
	 writef('-> %w\n', [R]).
