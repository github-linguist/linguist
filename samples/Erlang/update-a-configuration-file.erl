-module( update_configuration_file ).

-export( [add/3, change/3, disable/2, enable/2, read/1, task/0, write/2] ).

add( Option, Value, Lines ) ->
	Upper = string:to_upper( Option ),
	[string:join( [Upper, Value], " " ) | Lines].

change( Option, Value, Lines ) ->
	Upper = string:to_upper( Option ),
	change_done( Option, Value, Lines, [change_option(Upper, Value, X) || X <- Lines] ).

disable( Option, Lines ) ->
	Upper = string:to_upper( Option ),
	[disable_option(Upper, X) || X <- Lines].

enable( Option, Lines ) ->
	Upper = string:to_upper( Option ),
	[enable_option(Upper, X) || X <- Lines].

read( Name ) ->
	{ok, Binary} = file:read_file( Name ),
	Lines = [binary:bin_to_list(X) || X <- binary:split( Binary, <<"\n">>, [global] )],
	Lines_no_white = [string:strip(X) || X <- Lines],
	Lines_no_control = [strip_control(X) || X <- Lines_no_white],
	Lines_no_consecutive_space = [string:join(string:tokens(X, " "), " ") || X <- Lines_no_control],
	Lines_no_consecutive_semicolon = [strip_semicolon(X) || X <- Lines_no_consecutive_space],
	Lines_no_empty = lists:filter( fun strip_empty/1, Lines_no_consecutive_semicolon ),
	Lines_upper = [to_upper(X) || X <- Lines_no_empty],
	lists:reverse( lists:foldl(fun remove_duplicates/2, [], Lines_upper) ).

task() ->
	Lines = read( "priv/configuration_file2" ),
	Disabled_lines = disable( "needspeeling", Lines ),
	Enabled_lines = enable( "SEEDSREMOVED", Disabled_lines ),
	Changed_lines1 = change( "NUMBEROFBANANAS", "1024", Enabled_lines ),
	Changed_lines2 = change( "numberofstrawberries", "62000", Changed_lines1 ),
	write( "configuration_file", Changed_lines2 ),
	[io:fwrite( "Wrote this line: ~s~n", [X]) || X <- Changed_lines2].

write( Name, Lines ) -> file:write_file( Name, binary:list_to_bin(string:join(Lines, "\n")) ).



change_done( Option, Value, Lines, Lines ) -> add( Option, Value, Lines );
change_done( _Option, _Value, _Lines, New_lines ) -> New_lines.

change_option( Option, Value, String ) -> change_option_same( string:str(String, Option), Value, String ).

change_option_same( 1, Value, String ) ->
	[Option | _T] = string:tokens( String, " " ),
	string:join( [Option, Value], " " );
change_option_same( _N, _Value, String ) -> String.

disable_option( Option, String ) -> disable_option_same( string:str(String, Option), String ).

disable_option_same( 1, String ) -> "; " ++ String;
disable_option_same( _N, String ) -> String.

enable_option( Option, String ) -> enable_option_same( string:str(String, "; " ++ Option), String ).

enable_option_same( 1, "; " ++ String ) -> String;
enable_option_same( _N, String ) -> String.

is_semicolon( $; ) -> true;
is_semicolon( _C ) -> false.

remove_duplicates( "#" ++_T=Line, Lines ) -> [Line | Lines];
remove_duplicates( Line, Lines ) ->
	Duplicates = [X || X <-Lines, 1 =:= string:str(Line, X)],
	remove_duplicates( Duplicates, Line, Lines ).

remove_duplicates( [], Line, Lines ) -> [Line | Lines];
remove_duplicates( _Duplicates, _Line, Lines ) -> Lines.

strip_control( "" ) -> "";
strip_control( ";" ++ _T=String ) -> lists:filter( fun strip_control_codes:is_not_control_code_nor_extended_character/1, String );
strip_control( String ) -> String.

strip_empty( ";" ) -> false;
strip_empty( _String ) -> true.


strip_semicolon( ";" ++ _T=String ) -> ";" ++ lists:dropwhile( fun is_semicolon/1, String );
strip_semicolon( String ) -> String.

to_upper( "" ) -> "";
to_upper( "#" ++ _T=String ) -> String;
to_upper( "; " ++ _T=String ) ->
	[";", Option | T] = string:tokens( String, " " ),
	string:join( [";", string:to_upper(Option) | T], " " );
to_upper( String ) ->
	[Option | T] = string:tokens( String, " " ),
	string:join( [string:to_upper(Option) | T], " " ).
