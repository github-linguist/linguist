-module(luhn_test).

-export( [credit_card/1, task/0] ).

luhn_sum([Odd, Even |Rest]) when Even >= 5 ->
    Odd + 2 * Even - 10 + 1 + luhn_sum(Rest);
luhn_sum([Odd, Even |Rest]) ->
    Odd + 2 * Even + luhn_sum(Rest);
luhn_sum([Odd]) ->
    Odd;
luhn_sum([]) ->
    0.

check( Sum ) when (Sum rem 10) =:= 0 -> valid;
check( _Sum ) -> invalid.

credit_card(Digits) ->
    check(luhn_sum(lists:map(fun(D) -> D-$0 end, lists:reverse(Digits)))).

task() ->
    Numbers = ["49927398716", "49927398717", "1234567812345678", "1234567812345670"],
    [io:fwrite("~s: ~p~n", [X, credit_card(X)]) || X <- Numbers].
