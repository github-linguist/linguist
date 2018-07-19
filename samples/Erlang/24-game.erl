-module(g24).
-export([main/0]).

main() ->
    random:seed(now()),
    io:format("24 Game~n"),
    play().

play() ->
    io:format("Generating 4 digits...~n"),
    Digts = [random:uniform(X) || X <- [9,9,9,9]],
    io:format("Your digits\t~w~n", [Digts]),
    read_eval(Digts),
    play().

read_eval(Digits) ->
    Exp = string:strip(io:get_line(standard_io, "Your expression: "), both, $\n),
    case {correct_nums(Exp, Digits), eval(Exp)} of
        {ok, X} when X == 24 -> io:format("You Win!~n");
        {ok, X} -> io:format("You Lose with ~p!~n",[X]);
        {List, _} -> io:format("The following numbers are wrong: ~p~n", [List])
    end.

correct_nums(Exp, Digits) ->
    case re:run(Exp, "([0-9]+)", [global, {capture, all_but_first, list}]) of
        nomatch ->
            "No number entered";
        {match, IntLs} ->
            case [X || [X] <- IntLs, not lists:member(list_to_integer(X), Digits)] of
                [] -> ok;
                L -> L
            end
    end.

eval(Exp) ->
    {X, _} = eval(re:replace(Exp, "\\s", "", [{return, list},global]),
                  0),
    X.

eval([], Val) ->
    {Val,[]};
eval([$(|Rest], Val) ->
    {NewVal, Exp} = eval(Rest, Val),
    eval(Exp, NewVal);
eval([$)|Rest], Val) ->
    {Val, Rest};
eval([$[|Rest], Val) ->
    {NewVal, Exp} = eval(Rest, Val),
    eval(Exp, NewVal);
eval([$]|Rest], Val) ->
    {Val, Rest};
eval([$+|Rest], Val) ->
    {NewOperand, Exp} = eval(Rest, 0),
    eval(Exp, Val + NewOperand);
eval([$-|Rest], Val) ->
    {NewOperand, Exp} = eval(Rest, 0),
    eval(Exp, Val - NewOperand);
eval([$*|Rest], Val) ->
    {NewOperand, Exp} = eval(Rest, 0),
    eval(Exp, Val * NewOperand);
eval([$/|Rest], Val) ->
    {NewOperand, Exp} = eval(Rest, 0),
    eval(Exp, Val / NewOperand);
eval([X|Rest], 0) when X >= $1, X =< $9 ->
    eval(Rest, X-$0).
