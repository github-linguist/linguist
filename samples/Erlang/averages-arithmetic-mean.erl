mean([]) -> 0;
mean(L)  -> lists:sum(L)/erlang:length(L).
