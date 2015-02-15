-module(rosetta).
-export([ncs/1]).

masks(N) ->
    MaxMask = trunc(math:pow(2, N)),
    Total = lists:map(fun(X) -> integer_to_list(X, 2) end,
                lists:seq(3, MaxMask)),
    Filtered = lists:filter(fun(X) -> contains_noncont(X) end, Total),
    lists:map(fun(X) -> string:right(X, N, $0) end, Filtered). % padding

contains_noncont(N) ->
    case re:run(N, "10+1") of
        {match, _} -> true;
        nomatch -> false
    end.

apply_mask_to_list(Mask, List) ->
    Zipped = lists:zip(Mask, List),
    Filtered = lists:filter(fun({Include, _}) -> Include > 48 end, Zipped),
    lists:map(fun({_, Value}) -> Value end, Filtered).

ncs(List) ->
    lists:map(fun(Mask) -> apply_mask_to_list(Mask, List) end,
                masks(length(List))).
