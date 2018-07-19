-module( best_shuffle ).

-export( [sameness/2, string/1, task/0] ).

sameness( String1, String2 ) -> lists:sum( [1 || {X, X} <- lists:zip(String1, String2)] ).

string( String ) ->
	{"", String, Acc} = lists:foldl( fun different/2, {lists:reverse(String), String, []}, String ),
	lists:reverse( Acc ).

task() ->
	Strings = ["abracadabra", "seesaw", "elk", "grrrrrr", "up", "a"],
	Shuffleds = [string(X) || X <- Strings],
	[io:fwrite("~p ~p ~p~n", [X, Y, sameness(X,Y)]) || {X, Y} <- lists:zip(Strings, Shuffleds)].



different( Character, {[Character], Original, Acc} ) ->
	try_to_save_last( Character, Original, Acc );
different( Character, {[Character | T]=Not_useds, Original, Acc} ) ->
	Different_or_same = different_or_same( [X || X <- T, X =/= Character], Character ),
	{lists:delete(Different_or_same, Not_useds), Original, [Different_or_same | Acc]};
different( _Character1, {[Character2 | T], Original, Acc} ) ->
	{T, Original, [Character2 | Acc]}.

different_or_same( [Different | _T], _Character ) -> Different;
different_or_same( [], Character ) -> Character.

try_to_save_last( Character, Original_string, Acc ) ->
	Fun = fun ({X, Y}) -> (X =:= Y) orelse (X =:= Character) end,
	New_acc = try_to_save_last( lists:splitwith(Fun, lists:zip(lists:reverse(Original_string), [Character | Acc])), [Character | Acc] ),
	{"", Original_string, New_acc}.

try_to_save_last( {_Not_split, []}, Acc ) -> Acc;
try_to_save_last( {Last_reversed_zip, First_reversed_zip}, _Acc ) ->
	{_Last_reversed_original, [Last_character_acc | Last_part_acc]} = lists:unzip( Last_reversed_zip ),
	{_First_reversed_original, [Character_acc | First_part_acc]} = lists:unzip( First_reversed_zip ),
	[Character_acc | Last_part_acc] ++ [Last_character_acc | First_part_acc].
