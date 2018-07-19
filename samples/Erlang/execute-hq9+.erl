% hq9+ Erlang implementation (JWL)
% http://www.erlang.org/
-module(hq9p).
-export([main/1]).

%% bottle helper routine
bottle(0) ->
  io:format("No more bottles of beer ");

bottle(1) ->
  io:format("1 bottle of beer ");

bottle(N) when N > 0 ->
  io:format("~w bottles of beer ", [N]).

%% Implementation of instructions	
beer(0) ->
  bottle(0), io:format("on the wall~n"),
  bottle(0), io:format("on the wall~nGo to the store and buy some more~n"),
  io:format("99 bottles of beer on the wall.~n");
	
beer(N) ->
  bottle(N), io:format("on the wall~n"),
  bottle(N), io:format("~nTake one down and pass it around~n"),
  bottle(N-1), io:format("on the wall~n~n"),
  beer(N-1).

hello() ->
  io:format("Hello world!~n", []).

prog(Prog) ->
  io:format("~s~n", [Prog]).

inc(Acc) ->
  Acc+1.

%% Interpreter	
execute(Instruction, Prog, Acc) ->
  case Instruction of
    $H -> hello(), Acc;
    $Q -> prog(Prog), Acc;
    $9 -> beer(99), Acc;
    $+ -> inc(Acc);
       _ -> io:format("Invalid instruction: ~c~n", [Instruction]), Acc
  end.
	
main([], _Prog, Acc) ->
  Acc;
		
main([Instruction | Rest], Prog, Acc) ->
  NewAcc = execute(Instruction, Prog, Acc),
  main(Rest, Prog, NewAcc).
	
main(Prog) ->
  Compiled = string:to_upper(Prog),
  main(Compiled, Prog, 0).
