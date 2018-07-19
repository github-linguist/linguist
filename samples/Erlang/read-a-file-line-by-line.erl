-module( read_a_file_line_by_line ).

-export( [into_list/1] ).

into_list( File ) ->
        {ok, IO} = file:open( File, [read] ),
        into_list( io:get_line(IO, ''), IO, [] ).


into_list( eof, _IO, Acc ) -> lists:reverse( Acc );
into_list( {error, _Error}, _IO, Acc ) -> lists:reverse( Acc );
into_list( Line, IO, Acc ) -> into_list( io:get_line(IO, ''), IO, [Line | Acc] ).
