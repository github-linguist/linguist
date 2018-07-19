-module( singly_linked_list ).

-export( [append/2, foreach/2, free/1, insert/3, new/1, task/0] ).

append( New, Start ) -> Start ! {append, New}.

foreach( Fun, Start ) -> Start ! {foreach, Fun}.

free( Element ) -> Element ! {free}.

insert( New, After, Start ) -> Start ! {insert, New, After}.

new( Data ) -> erlang:spawn( fun() -> loop( Data, nonext ) end ).

task() ->
    A = new( a ),
    B = new( b ),
    append( B, A ),
    C = new( c ),
    insert( C, A, A ),
    foreach( fun(Data) -> io:fwrite("~p~n", [Data]) end, A ).



loop( Data, Next ) ->
      My_pid = erlang:self(),
      receive
      {append, New} ->
             New_next = loop_append( New, Next ),
             loop( Data, New_next );
      {foreach, Fun} ->
                catch Fun( Data ),
		loop_foreach( Fun, Next ),
                loop( Data, Next );
      {free} ->
             ok;
      {insert, New, My_pid} ->
             append( Next, New ),
             loop( Data, New );
      {insert, New, After} ->
             Next ! {insert, New, After},
             loop( Data, Next )
        end.

loop_append( New, nonext ) -> New;
loop_append( New, Next ) ->
        Next ! {append, New},
        Next.

loop_foreach( _Fun, nonext ) -> ok;
loop_foreach( Fun, Next ) -> Next ! {foreach, Fun}.
