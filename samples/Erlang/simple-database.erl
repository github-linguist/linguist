#! /usr/bin/env escript

-compile({no_auto_import,[date/0]}).

main( ["add", Tag | Descriptions] ) -> add( date(), Tag, Descriptions );
main( ["add_date", Date, Tag | Descriptions] ) -> add( date_internal(string:tokens(Date, "-")), Tag, Descriptions );
main( ["print_latest"] ) -> print_latest( contents() );
main( ["print_latest_for_each"] ) -> print_latest_for_each( contents() );
main( ["print_all_date", Date] ) -> print_all_date( date_internal(string:tokens(Date, "-")), contents() );
main( _Error ) -> usage().



add( Date, Tag, Descriptions ) ->
	Contents = contents(),
	file:write_file( file(), io_lib:format("simple_database_v1.~n~p.~n", [[{Date, Tag, Descriptions} | Contents]]) ).

date() ->
	{{Date, _Time}} = calendar:local_time(),
	Date.	

date_external( {Year, Month, Day} ) -> string:join( [erlang:integer_to_list(Year), erlang:integer_to_list(Month), erlang:integer_to_list(Day)], "-" );
date_external( _Error ) -> usage().

date_internal( [Year, Month, Day] ) -> {erlang:list_to_integer(Year), erlang:list_to_integer(Month), erlang:list_to_integer(Day)};
date_internal( _Error ) -> usage().

file() -> "simple_database_contents".

contents() -> contents( file:consult(file()) ).

contents( {ok, [simple_database_v1, Contents]} ) -> Contents;
contents( {error, Error} ) when is_atom(Error) -> [];
contents( {error, _Error} ) ->
	io:fwrite( "Error: ~p corrupt. Starting from scratch~n", [file()] ),
	[].

print_all_date( _Date, [] ) -> ok;
print_all_date( Date, Contents ) -> [print_latest([{D, Tag, Descriptions}]) || {D, Tag, Descriptions} <- Contents, D =:= Date].

print_latest( [] ) -> ok;
print_latest( [{Date, Tag, Descriptions} | _T] ) -> io:fwrite( "~s~n", [string:join( [date_external(Date), Tag | Descriptions], " ")] ).

print_latest_for_each( [] ) -> ok;
print_latest_for_each( Contents ) ->
	Tags = lists:usort( [Tag || {_Date, Tag, _Descriptions} <- Contents] ),
	[print_latest([lists:keyfind(X, 2, Contents)]) ||  X <- Tags].

usage() ->
	io:fwrite( "Usage: ~p [add | add_date <date>]  tag description ...~n", [escript:script_name()] ),
	io:fwrite( "Or: ~p [print_latest | print_latest_for_each | print_all_date <date>]~n", [escript:script_name()] ),
	io:fwrite( "Date format is YYYY-MM-DD~n" ),
	io:fwrite( "Data stored in ~p~n", [file()] ),
	init:stop().
