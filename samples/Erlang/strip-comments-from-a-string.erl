-module( strip_comments_from_string ).

-export( [task/0] ).

task() ->
    io:fwrite( "~s~n", [keep_until_comment("apples, pears and bananas")] ),
    io:fwrite( "~s~n", [keep_until_comment("apples, pears # and bananas")] ),
    io:fwrite( "~s~n", [keep_until_comment("apples, pears ; and bananas")] ).



keep_until_comment( String ) ->	lists:takewhile( fun not_comment/1, String ).

not_comment( $# ) -> false;
not_comment( $; ) -> false;
not_comment( _ ) -> true.
