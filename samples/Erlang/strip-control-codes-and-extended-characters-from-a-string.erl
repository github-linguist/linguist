-module( strip_control_codes ).

-export( [is_not_control_code/1, is_not_control_code_nor_extended_character/1, task/0] ).

is_not_control_code( C ) when C > 127 -> true;
is_not_control_code( C ) when C < 32; C =:= 127 -> false;
is_not_control_code( _C ) -> true.

is_not_control_code_nor_extended_character( C ) when C > 127 -> false;
is_not_control_code_nor_extended_character( C )	-> is_not_control_code( C ).

task() ->
    String = lists:seq( 0, 255 ),
    io:fwrite( "String (~p characters): ~s~n", [erlang:length(String), String] ),
    String_without_cc = lists:filter( fun is_not_control_code/1, String ),
    io:fwrite( "String without control codes (~p characters): ~s~n", [erlang:length(String_without_cc), String_without_cc] ),
    String_without_cc_nor_ec = lists:filter( fun is_not_control_code_nor_extended_character/1, String ),
    io:fwrite( "String without control codes nor extended characters (~p characters): ~s~n", [erlang:length(String_without_cc_nor_ec), String_without_cc_nor_ec] ).
