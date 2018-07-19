-module( active_object ).
-export( [delete/1, input/2, new/0, output/1, task/1] ).
-compile({no_auto_import,[time/0]}).

delete( Object ) ->
      Object ! stop.

input( Object, Fun ) ->
      Object ! {input, Fun}.

new( ) ->
      K = fun zero/1,
      S = 0,
      T0 = seconds_with_decimals(),
      erlang:spawn( fun() -> loop(K, S, T0) end ).

output( Object ) ->
      Object ! {output, erlang:self()},
      receive
      {output, Object, Output} -> Output
      end.

task( Integrate_millisec ) ->
      Object = new(),
      {ok, _Ref} = timer:send_interval( Integrate_millisec, Object, integrate ),
      io:fwrite( "New ~p~n", [output(Object)] ),
      input( Object, fun sine/1 ),
      timer:sleep( 2000 ),
      io:fwrite( "Sine ~p~n", [output(Object)] ),
      input( Object, fun zero/1 ),
      timer:sleep( 500 ),
      io:fwrite( "Approx ~p~n", [output(Object)] ),
      delete( Object ).



loop( Fun, Sum, T0 ) ->
      receive
      integrate ->
                T1 = seconds_with_decimals(),
                New_sum = trapeze( Sum, Fun, T0, T1 ),
                loop( Fun, New_sum, T1 );
      stop ->
                ok;
      {input, New_fun} ->
		loop( New_fun, Sum, T0 );
      {output, Pid} ->
                Pid ! {output, erlang:self(), Sum},
                loop( Fun, Sum, T0 )
      end.

sine( T ) ->
      math:sin( 2 * math:pi() * 0.5 * T ).

seconds_with_decimals() ->
      {Megaseconds, Seconds, Microseconds} = os:timestamp(),
      (Megaseconds * 1000000) + Seconds + (Microseconds / 1000000).

trapeze( Sum, Fun, T0, T1 ) ->
      Sum + (Fun(T1) + Fun(T0)) * (T1 - T0) / 2.

zero( _ ) -> 0.
