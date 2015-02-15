-module(delete).
-export([main/0]).

main() ->
	% current directory
        ok = file:del_dir( "docs" ),
	ok = file:delete( "input.txt" ),
	% root directory
	ok = file:del_dir( "/docs" ),
	ok = file:delete( "/input.txt" ).
