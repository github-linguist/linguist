-module( inverted_index ).

-export( [from_files/1, search/2, task/0] ).

from_files( Files ) ->
        lists:foldl( fun import_from_file/2, dict:new(), Files ).

search(	Binaries, Inverted_index ) ->
        [Files | T] = [dict:fetch(X, Inverted_index) || X <- Binaries],
        lists:foldl( fun search_common/2, Files, T ).

task() ->
       Files_contents = [{"file_1", <<"it is what it is">>}, {"file_2", <<"what is it">>}, {"file_3", <<"it is a banana">>}],
       [file:write_file(X, Y) || {X, Y} <- Files_contents],
       Inverted_index = from_files( [X || {X, _Y} <- Files_contents] ),
       Result = search( [<<"what">>, <<"is">>, <<"it">>], Inverted_index ),
       io:fwrite( "~p~n", [Result] ),
       [file:delete(X) || {X, _Y} <- Files_contents].



import_from_file( File, Dict_acc ) ->
        New_dict = dict:from_list( import_from_file_contents(File, file:read_file(File)) ),
	dict:merge( fun import_from_file_merge/3, Dict_acc, New_dict ).

import_from_file_contents( File, {ok, Binary} ) ->
        [{X, [File]} || X <- binary:split( Binary, binary:compile_pattern([<<" ">>, <<"\n">>]), [global] )];
import_from_file_contents( File, {error, Error} ) ->
	io:fwrite( "Error: could not open file ~p: ~p~nContinuing with the rest of them~n", [File,	Error] ),
	[].

import_from_file_merge(	_Key, Files, [New_file] ) -> [New_file | Files].

search_common( Files, Acc ) -> [X || X <- Acc, lists:member(X, Files)].
