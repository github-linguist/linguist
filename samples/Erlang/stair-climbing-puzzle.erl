-module(stair).
-compile(export_all).

step() ->
    1 == random:uniform(2).

step_up(true) ->
    ok;
step_up(false) ->
    step_up(step()),
    step_up(step()).

step_up() ->
    step_up(step()).
