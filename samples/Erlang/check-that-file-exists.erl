#!/usr/bin/escript
existence( true ) ->"exists";
existence( false ) ->"does not exist".

print_result(Type, Name, Flag) -> io:fwrite( "~s ~s ~s~n", [Type, Name, existence(Flag)] ).


main(_) ->
        print_result( "File", "input.txt", filelib:is_regular("input.txt") ),
        print_result( "Directory", "docs", filelib:is_dir("docs") ),
        print_result( "File", "/input.txt", filelib:is_regular("/input.txt") ),
        print_result( "Directory", "/docs", filelib:is_dir("/docs") ).
