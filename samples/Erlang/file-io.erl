-module( file_io ).

-export( [task/0] ).

task() ->
       {ok, Contents} = file:read_file( "input.txt" ),
       ok = file:write_file( "output.txt", Contents ).
