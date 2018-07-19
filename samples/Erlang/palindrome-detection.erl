-module( palindrome ).

-export( [is_palindrome/1, task/0] ).

is_palindrome( String ) -> String =:= lists:reverse(String).

task() ->
	display( "abcba" ),
	display( "abcdef" ),
	Latin = "In girum imus nocte et consumimur igni",
	No_spaces_same_case = lists:append( string:tokens(string:to_lower(Latin), " ") ),
	display( Latin, No_spaces_same_case ).



display( String ) -> io:fwrite( "Is ~p a palindrom? ~p~n", [String, is_palindrome(String)] ).

display( String1, String2 ) -> io:fwrite( "Is ~p a palindrom? ~p~n", [String1, is_palindrome(String2)] ).
