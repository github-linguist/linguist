% Implemented by Arjun Sunel
-module(pick_random).
-export([main/0]).

main() ->
	List =[1,2,3,4,5],
	index = random:uniform(length(List)),
	lists:nth(index,List).
