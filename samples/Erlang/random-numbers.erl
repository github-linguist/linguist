mean(Values) ->
    mean(tl(Values), hd(Values), 1).

mean([], Acc, Length) ->
    Acc / Length;
mean(Values, Acc, Length) ->
    mean(tl(Values), hd(Values)+Acc, Length+1).

variance(Values) ->
    Mean = mean(Values),
    variance(Values, Mean, 0) / length(Values).

variance([], _, Acc) ->
    Acc;
variance(Values, Mean, Acc) ->
    Diff = hd(Values) - Mean,
    DiffSqr = Diff * Diff,
    variance(tl(Values), Mean, Acc + DiffSqr).

stddev(Values) ->
    math:sqrt(variance(Values)).

normal(Mean, StdDev) ->
    U = random:uniform(),
    V = random:uniform(),
    Mean + StdDev * ( math:sqrt(-2 * math:log(U)) * math:cos(2 * math:pi() * V) ).  % Erlang's math:log is the natural logarithm.

main(_) ->
    X = [ normal(1.0, 0.5) || _ <- lists:seq(1, 1000) ],
    io:format("mean = ~w\n", [mean(X)]),
    io:format("stddev = ~w\n", [stddev(X)]).
