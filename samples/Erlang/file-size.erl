-module(file_size).
-export([file_size/0]).

-include_lib("kernel/include/file.hrl").

file_size() ->
    print_file_size("input.txt"),
    print_file_size("/input.txt").

print_file_size(Filename) ->
    case file:read_file_info(Filename) of
	{ok, FileInfo} ->
	    io:format("~s ~p~n", [Filename, FileInfo#file_info.size]);
	{error, _} ->
	    io:format("~s could not be opened~n",[Filename])
    end.
