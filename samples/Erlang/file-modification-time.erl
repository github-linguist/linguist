-module( file_modification_time ).

-include_lib("kernel/include/file.hrl").

-export( [task/0] ).

task() ->
       File = "input.txt",
       {ok, File_info} = file:read_file_info( File ),
       io:fwrite( "Modification time ~p~n", [File_info#file_info.mtime] ),
       ok = file:write_file_info( File, File_info#file_info{mtime=calendar:local_time()} ).
