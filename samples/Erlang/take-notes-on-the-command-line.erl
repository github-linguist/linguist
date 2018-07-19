#! /usr/bin/env escript

main( Arguments ) ->
	display_notes( Arguments ),
	save_notes( Arguments ).



display_notes( [] ) -> io:fwrite( "~s", [erlang:binary_to_list(file_contents())] );
display_notes( _Arguments ) -> ok.

file() -> "NOTES.TXT".

file_contents() -> file_contents( file:read_file(file()) ).

file_contents( {ok, Binary} ) -> Binary;
file_contents( {error, _Error} ) -> <<>>.

save_notes( [] ) -> ok;
save_notes( Arguments ) ->
	Date = io_lib:format( "~p~n", [calendar:local_time()] ),
	Notes = [Date ++ "\t" | [io_lib:format( "~s ", [X]) || X <- Arguments]],
	Existing_contents = file_contents(),
	file:write_file( file(), [Existing_contents, Notes, "\n"] ).
