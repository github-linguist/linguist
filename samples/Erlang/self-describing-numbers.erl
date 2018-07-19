sdn(N) -> lists:map(fun(S)->length(lists:filter(fun(C)->C-$0==S end,N))+$0 end,lists:seq(0,length(N)-1))==N.
gen(M) -> lists:filter(fun(N)->sdn(integer_to_list(N)) end,lists:seq(0,M)).
