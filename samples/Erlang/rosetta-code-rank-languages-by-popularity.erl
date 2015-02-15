-module( rank_languages_by_popularity ).

-export( [task/0] ).

-record( print_fold, {place=0, place_step=1, previous_count=0} ).

task() ->
	ok = find_unimplemented_tasks:init(),
	Category_programming_languages = find_unimplemented_tasks:rosetta_code_list_of( "Programming_Languages" ),
	Programming_languages = [X || "Category:" ++ X <- Category_programming_languages],
	{ok, {{_HTTP,200,"OK"}, _Headers, Body}} = httpc:request( "http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000" ),
	Count_categories = lists:sort( [{Y, X} || {X, Y} <- category_counts(Body, []), lists:member(X, Programming_languages)] ),
	lists:foldr( fun place_count_category_write/2, #print_fold{}, Count_categories ).



category_counts( "", [[] | Acc] ) -> Acc;
category_counts( String, Acc ) ->
	{Begin, End} = category_count_begin_end( String ),
	{Category_count, String_continuation} = category_count_extract( String, Begin, End ),
	category_counts( String_continuation, [Category_count | Acc] ).

category_count_begin_end( String ) ->
	Begin = string:str( String, "/wiki/Category:" ),
	End = string:str( string:substr(String, Begin), " member" ),
	category_count_begin_end( Begin, End, erlang:length(" member") ).

category_count_begin_end( _Begin, 0, _End_length ) -> {0, 0};
category_count_begin_end( Begin, End, End_length ) ->
	{Begin, Begin + End + End_length}.

category_count_extract( _String, 0, _End ) -> {[], ""};
category_count_extract( String, Begin, End ) ->
	Category_count = category_count_extract( string:substr(String, Begin, End - Begin) ),
	{Category_count, string:substr( String, End + 1 )}.

category_count_extract( "/wiki/Category:" ++ T ) ->
	Category_member = string:tokens( T, " " ),
	Category = category_count_extract_category( Category_member ),
	Member = category_count_extract_count( lists:reverse(Category_member) ),
	{Category, Member}.

category_count_extract_category( [Category | _T] ) ->
	lists:map( fun category_count_extract_category_map/1, string:strip(Category, right, $") ).

category_count_extract_category_map( $_ ) -> $\s;
category_count_extract_category_map( Character ) -> Character.

category_count_extract_count( ["member" ++ _, "(" ++ N | _T] ) -> erlang:list_to_integer( N );
category_count_extract_count( _T ) -> 0.

place_count_category_write( {Count, Category}, Acc ) ->
	Print_fold = place_count_category_write( Count, Acc ),
	io:fwrite("~p. ~p - ~p~n", [Print_fold#print_fold.place, Count, Category] ),
	Print_fold;

place_count_category_write( Count, #print_fold{place_step=Place_step, previous_count=Count}=Print_fold ) ->
	Print_fold#print_fold{place_step=Place_step + 1};
place_count_category_write( Count, #print_fold{place=Place, place_step=Place_step} ) ->
	#print_fold{place=Place + Place_step, previous_count=Count}.
