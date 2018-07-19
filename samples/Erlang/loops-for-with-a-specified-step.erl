%% Implemented by Arjun Sunel
%% for_loop/4 by Bengt Kleberg.
-module(loop_step).
-export([main/0, for_loop/1, for_loop/4]).

 % This Erlang code for "For Loop" is equivalent to: " for (i=start;  i<end ; i=i+2){ printf("* ");} " in C language.

main() ->
	for_loop(1).

 for_loop( N ) ->
	for_loop( N, 4, 2, fun() -> io:fwrite("* ") end ).

for_loop( I, End, Step, Do ) when N < End ->
    Do(),
    for_loop( I+Step, End, Step, Do );
for_loop( _I, _End, _Step, _Do ) -> ok.
