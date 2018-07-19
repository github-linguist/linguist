%% Compile
%%
%% erlc scriptname.erl
%%
%% Run
%%
%% erl -noshell -s scriptname

-module(scriptname).
-export([start/0]).

start() ->
  Program = ?FILE,
  io:format("Program: ~s~n", [Program]),
  init:stop().
