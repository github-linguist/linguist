-module( globally_replace_text ).

-export( [in_files/3, main/1] ).

in_files( Old, New, Files ) when is_list(Old) ->
        in_files( binary:list_to_bin(Old), binary:list_to_bin(New), Files );
in_files( Old, New, Files ) -> [replace_in_file(Old, New, X, file:read_file(X)) || X <- Files].

main( [Old, New | Files] ) -> in_files( Old, New, Files ).



replace_in_file( Old, New, File, {ok, Binary} ) ->
	replace_in_file_return( File, file:write_file(File, binary:replace(Binary, Old, New, [global])) );
replace_in_file( _Old, _New, File, {error, Error} ) ->
        io:fwrite( "Error: Could not read ~p: ~p~n", [File, Error] ),
        error.

replace_in_file_return( _File, ok ) -> ok;
replace_in_file_return( File, {error, Error} ) ->
	io:fwrite( "Error: Could not write ~p: ~p~n", [File, Error] ),
        error.
