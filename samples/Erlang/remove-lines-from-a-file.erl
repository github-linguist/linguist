-module( remove_lines ).

-export( [from_file/3, task/0] ).

from_file( Name, Start, How_many ) ->
	{Name, {ok, Binary}} = {Name, file:read_file( Name )},
	Lines = compensate_for_last_newline( lists:reverse([X || X <- binary:split( Binary, <<"\n">>, [global] )]) ),
	{Message, Keep_lines} = keep_lines( Start - 1, How_many, Lines, erlang:length(Lines) - 1 ),
	ok = file:write_file( Name, [binary:list_to_bin([X, <<"\n">>]) || X <- Keep_lines] ),
	io:fwrite( "~s~n", [Message] ).

task() ->
	file:copy( "priv/foobar.txt", "foobar.txt" ),
	from_file( "foobar.txt", 1, 2 ).



compensate_for_last_newline( [<<>> | T] ) -> lists:reverse( T );
compensate_for_last_newline( Reversed_lines ) -> lists:reverse( Reversed_lines ).

keep_lines( Start, _How_many, Lines, Available ) when Start > Available ->
	{"Start > avaliable. Nothing removed.", Lines};
keep_lines( Start, How_many, Lines, Available ) when Start + How_many  > Available ->
	Message = lists:flatten( io_lib:format("Start + How_many > avaliable. Only ~p removed.", [(Start + How_many) - Available]) ),
	{Keeps, _Removes} = lists:split( Start, Lines ),
	{Message, Keeps};
keep_lines( Start, How_many, Lines, _Available ) ->
	{Keeps, Removes} = lists:split( Start, Lines ),
	{"ok", Keeps ++ lists:nthtail( How_many, Removes )}.
