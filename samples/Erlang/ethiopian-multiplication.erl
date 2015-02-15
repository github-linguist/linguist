-module(ethopian).
-export([multiply/2]).

halve(N) ->
    N div 2.

double(N) ->
    N * 2.

even(N) ->
    (N rem 2) == 0.

multiply(LHS,RHS) when is_integer(Lhs) and Lhs > 0 and
			is_integer(Rhs) and Rhs > 0 ->
    multiply(LHS,RHS,0).

multiply(1,RHS,Acc) ->
    RHS+Acc;
multiply(LHS,RHS,Acc) ->
    case even(LHS) of
        true ->
            multiply(halve(LHS),double(RHS),Acc);
        false ->
            multiply(halve(LHS),double(RHS),Acc+RHS)
    end.
