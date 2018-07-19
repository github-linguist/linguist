-module( find_common_directory ).

-export( [path/2, task/0] ).

path( [Path | T], _Separator ) -> filename:join( lists:foldl(fun keep_common/2, filename:split(Path), [filename:split(X) || X <- T]) ).

task() -> path( ["/home/user1/tmp/coverage/test", "/home/user1/tmp/covert/operator", "/home/user1/tmp/coven/members"], "/" ).



keep_common( Components, Acc ) -> [X || X <- Components, Y <- Acc, X =:= Y].
