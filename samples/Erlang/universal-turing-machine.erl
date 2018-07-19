#!/usr/bin/env escript

-module(turing).
-mode(compile).

-export([main/1]).

% Incrementer definition:
% States: a | halt
% Initial state: a
% Halting states: halt
% Symbols: b | '1'
% Blank symbol: b
incrementer_config() -> {a, [halt], b}.
incrementer(a, '1') -> {'1', right, a};
incrementer(a, b)   -> {'1', stay, halt}.

% Busy beaver definition:
% States: a | b | c | halt
% Initial state: a
% Halting states: halt
% Symbols: '0' | '1'
% Blank symbol: '0'
busy_beaver_config() -> {a, [halt], '0'}.
busy_beaver(a, '0') -> {'1', right, b};
busy_beaver(a, '1') -> {'1', left, c};
busy_beaver(b, '0') -> {'1', left, a};
busy_beaver(b, '1') -> {'1', right, b};
busy_beaver(c, '0') -> {'1', left, b};
busy_beaver(c, '1') -> {'1', stay, halt}.

% Mainline code.
main([]) ->
    io:format("==============================~n"),
    io:format("Turing machine simulator test.~n"),
    io:format("==============================~n"),

    Tape1 = turing(fun incrementer_config/0, fun incrementer/2, ['1','1','1']),
    io:format("~w~n", [Tape1]),

    Tape2 = turing(fun busy_beaver_config/0, fun busy_beaver/2, []),
    io:format("~w~n", [Tape2]).

% Universal Turing machine simulator.
turing(Config, Rules, Input) ->
    {Start, _, _} = Config(),
    {Left, Right} = perform(Config, Rules, Start, {[], Input}),
    lists:reverse(Left) ++ Right.

perform(Config, Rules, State, Input = {LeftInput, RightInput}) ->
    {_, Halts, Blank} = Config(),
    case lists:member(State, Halts) of
        true  -> Input;
        false ->
            {NewRight, Symbol} = symbol(RightInput, Blank),
            {NewSymbol, Action, NewState} = Rules(State, Symbol),
            NewInput = action(Action, Blank, {LeftInput, [NewSymbol| NewRight]}),
            perform(Config, Rules, NewState, NewInput)
    end.

symbol([], Blank) -> {[], Blank};
symbol([S|R], _)  -> {R, S}.

action(left, Blank, {[], Right}) -> {[], [Blank|Right]};
action(left, _, {[L|Ls], Right}) -> {Ls, [L|Right]};
action(stay, _, Tape)            -> Tape;
action(right, Blank, {Left, []}) -> {[Blank|Left], []};
action(right, _, {Left, [R|Rs]}) -> {[R|Left], Rs}.
