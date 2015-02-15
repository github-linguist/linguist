% Implemented by Arjun Sunel
-module(arith).
-export([start/0]).

start() ->
   case io:fread("","~d~d") of
       {ok, [A,B]} ->
           io:format("Sum = ~w~n",[A+B]),
           io:format("Difference = ~w~n",[A-B]),
           io:format("Product = ~w~n",[A*B]),
           io:format("Quotient = ~w~n",[A div B]),      % truncates towards zero
           io:format("Remainder= ~w~n",[A rem B]),    % same sign as the first operand
           halt()
   end.
