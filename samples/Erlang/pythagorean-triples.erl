%%
%% Pythagorian triples in Erlang, J.W. Luiten
%%
-module(triples).
-export([main/1]).

%% Transformations t1, t2 and t3 to generate new triples
t1(A, B, C) ->
  {A-2*B+2*C, 2*A-B+2*C, 2*A-2*B+3*C}.
t2(A, B, C) ->
  {A+2*B+2*C, 2*A+B+2*C, 2*A+2*B+3*C}.
t3(A, B, C) ->
  {2*B+2*C-A, B+2*C-2*A, 2*B+3*C-2*A}.

%% Generation of triples
count_triples(A, B, C, Tot_acc, Cnt_acc, Max_perimeter) when (A+B+C) =< Max_perimeter ->
  Tot1 = Tot_acc + Max_perimeter div (A+B+C),
  {A1, B1, C1} = t1(A, B, C),
  {Tot2, Cnt2} = count_triples(A1, B1, C1, Tot1, Cnt_acc+1, Max_perimeter),

  {A2, B2, C2} = t2(A, B, C),
  {Tot3, Cnt3} = count_triples(A2, B2, C2, Tot2, Cnt2, Max_perimeter),

  {A3, B3, C3} = t3(A, B, C),
  {Tot4, Cnt4} = count_triples(A3, B3, C3, Tot3, Cnt3, Max_perimeter),
  {Tot4, Cnt4};
count_triples(_A, _B, _C, Tot_acc, Cnt_acc, _Max_perimeter) ->
  {Tot_acc, Cnt_acc}.

count_triples(A, B, C, Pow) ->
  Max = trunc(math:pow(10, Pow)),
  {Tot, Prim} = count_triples(A, B, C, 0, 0, Max),
  {Pow, Tot, Prim}.

count_triples(Pow) ->
  count_triples(3, 4, 5, Pow).

%% Display a single result.
display_result({Pow, Tot, Prim}) ->
  io:format("Up to 10 ** ~w : ~w triples, ~w primitives~n", [Pow, Tot, Prim]).

main(Max) ->
  L = lists:seq(1, Max),
  Answer = lists:map(fun(X) -> count_triples(X) end, L),
  lists:foreach(fun(Result) -> display_result(Result) end, Answer).
