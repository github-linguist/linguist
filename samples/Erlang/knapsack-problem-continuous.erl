-module( knapsack_problem_continuous ).

-export( [price_per_weight/1, select/2, task/0] ).

price_per_weight( Items ) -> [{Name, Weight, Price / Weight} || {Name, Weight, Price} <-Items].

select( Max_weight, Items ) ->
	{_Remains, Selected_items} = lists:foldr( fun select_until/2, {Max_weight, []}, lists:keysort(3, Items) ),
	Selected_items.

task() ->
	Items = items(),
	io:fwrite( "The robber takes the following to maximize the value~n" ),
	[io:fwrite("~.2f of ~p~n", [Weight, Name]) || {Name, Weight} <- select( 15, price_per_weight(Items) )].



items() ->
	[{"beef", 3.8, 36},
	{"pork", 5.4, 43},
	{"ham", 3.6, 90},
	{"greaves", 2.4, 45},
	{"flitch", 4.0, 30},
	{"brawn", 2.5, 56},
	{"welt", 3.7 , 67},
	{"salami", 3.0, 95},
	{"sausage",  5.9 , 98}
	].

select_until( {Name, Weight, _Price}, {Remains, Acc} ) when Remains > 0 ->
	Selected_weight = select_until_weight( Weight, Remains ),
	{Remains - Selected_weight, [{Name, Selected_weight} | Acc]};
select_until( _Item, Acc ) -> Acc.

select_until_weight( Weight, Remains ) when Weight < Remains -> Weight;
select_until_weight( _Weight, Remains ) -> Remains.
