-module(new_file).
-export([main/0]).

main() ->
	ok = file:write_file( "output.txt", <<>> ),
	ok = file:make_dir( "docs" ),
	ok = file:write_file( filename:join(["/", "output.txt"]), <<>> ),
	ok = file:make_dir( filename:join(["/", "docs"]) ).
