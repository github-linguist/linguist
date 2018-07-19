-module(arbitrary).
-compile([export_all]).

pow(B,E) when E > 0 ->
    pow(B,E,1).

pow(_,0,_) -> 0;
pow(B,1,Acc) -> Acc * B;
pow(B,P,Acc) when P rem 2 == 0 ->
    pow(B*B,P div 2, Acc);
pow(B,P,Acc) ->
    pow(B,P-1,Acc*B).

test() ->
    I = pow(5,pow(4,pow(3,2))),
    [S] = io_lib:format("~b",[I]),
    L = length(S),
    Prefix = lists:sublist(S,20),
    Suffix = lists:sublist(S,L-19,20),
    io:format("Length: ~b~nPrefix:~s~nSuffix:~s~n",[L,Prefix,Suffix]).
