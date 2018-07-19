-module( read_a_specific_line ).

-export( [from_file/2, task/0] ).

from_file( File, N ) -> line_nr( N, read_a_file_line_by_line:into_list(File) ).

task() ->
       Lines = read_a_file_line_by_line:into_list( "read_a_specific_line.erl" ),
       Line_7 = line_nr( 7, Lines ),
       Line_7.



line_nr( N, Lines ) ->
         try
         case lists:nth( N, Lines )
         of "\n" -> erlang:exit( empty_line )
         ; Line -> Line
         end

         catch
         _Type:Error0 ->
                Error = line_nr_error( Error0 ),
                io:fwrite( "Error: ~p~n", [Error] ),
                erlang:exit( Error )

         end.

line_nr_error( function_clause ) -> too_few_lines_in_file;
line_nr_error( Error ) -> Error.
