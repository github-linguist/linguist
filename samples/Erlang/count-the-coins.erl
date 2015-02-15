-module(coins).
-compile(export_all).

count(Amount, Coins) ->
    {N,_C} = count(Amount, Coins, dict:new()),
    N.

count(0,_,Cache) ->
    {1,Cache};
count(N,_,Cache) when N < 0 ->
    {0,Cache};
count(_N,[],Cache) ->
    {0,Cache};
count(N,[C|Cs]=Coins,Cache) ->
    case dict:is_key({N,length(Coins)},Cache) of
        true ->
            {dict:fetch({N,length(Coins)},Cache), Cache};
        false ->
            {N1,C1} = count(N-C,Coins,Cache),
            {N2,C2} = count(N,Cs,C1),
            {N1+N2,dict:store({N,length(Coins)},N1+N2,C2)}
    end.

print(Amount, Coins) ->
    io:format("~b ways to make change for ~b cents with ~p coins~n",[count(Amount,Coins),Amount,Coins]).

test() ->
    A1 = 100, C1 = [25,10,5,1],
    print(A1,C1),
    A2 = 100000, C2 = [100, 50, 25, 10, 5, 1],
    print(A2,C2).
