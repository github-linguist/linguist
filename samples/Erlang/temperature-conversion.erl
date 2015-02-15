% Implemented by Arjun Sunel
-module(temp_conv).
-export([main/0]).

main() ->
	conversion(21).

conversion(T) ->
	io:format("\nK : ~p\n\n",[f(T)]),
	io:format("C : ~p \n\n",[f(T - 273.15)]),
	io:format("F : ~p\n\n",[f(T * 1.8 - 459.67)]),
	io:format("R : ~p\n\n",[f(T * 1.8)]).

f(A) ->
	(round(A*100))/100 .	
