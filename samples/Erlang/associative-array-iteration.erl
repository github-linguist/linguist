-module(assoc).
-compile([export_all]).

test_create() ->
    D = dict:new(),
    D1 = dict:store(foo,1,D),
    D2 = dict:store(bar,2,D1),
    print_vals(D2).

print_vals(D) ->
    lists:foreach(fun (K) ->
                          io:format("~p: ~b~n",[K,dict:fetch(K,D)])
                  end, dict:fetch_keys(D)).
