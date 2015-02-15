-module(cc).

-export([start/0]).

start() ->
   My_pid = erlang:self(),
   Pid = erlang:spawn( fun() -> reader(My_pid, 0) end ),
   {ok, IO } = file:open( "input.txt", [read] ),
   process( io:get_line(IO, ""), IO, Pid ),
   file:close( IO ).



process( eof, _IO, Pid ) ->
   Pid ! count,
   receive
       I -> io:fwrite("Count:~p~n", [I])
   end;
process( Any, IO, Pid ) ->
   Pid ! Any,
   process( io:get_line(IO, ""), IO, Pid ).

reader(Pid, C) ->
   receive
       count -> Pid ! C;
       Any ->
           io:fwrite("~s", [Any]),
           reader(Pid, C+1)
   end.
