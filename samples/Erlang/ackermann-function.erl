-module(ack).
-export([main/1, ack/2]).

main( [A, B] ) ->
   io:fwrite( "~p~n",[ack(erlang:list_to_integer(A), erlang:list_to_integer(B))] ).

ack(0,N) -> N + 1;
ack(M,0) -> ack(M-1, 1);
ack(M,N) -> ack(M-1,ack(M,N-1)).
