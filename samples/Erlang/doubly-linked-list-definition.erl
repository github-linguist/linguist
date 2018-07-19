-module( doubly_linked_list ).

-export( [append/2, foreach_next/2, foreach_previous/2, free/1, insert/3, new/1, task/0] ).

append( New, Start ) -> Start ! {append, New}.

foreach_next( Fun, Start ) -> Start ! {foreach_next, Fun}.

foreach_previous( Fun, Start ) -> Start ! {foreach_previous, Fun}.

free( Element ) -> Element ! {free}.

insert( New, After, Start ) -> Start ! {insert, New, After}.

new( Data ) -> erlang:spawn( fun() -> loop( Data, noprevious, nonext ) end ).

task() ->
    A = new( a ),
    B = new( b ),
    append( B, A ),
    C = new( c ),
    insert( C, A, A ),
    foreach_next( fun(Data) -> io:fwrite("foreach_next ~p~n", [Data]) end, A ),
    timer:sleep( 100 ),
    foreach_previous( fun(Data) -> io:fwrite("foreach_previous ~p~n", [Data]) end, B ).



loop( Data, Previous, Next ) ->
      My_pid = erlang:self(),
      receive
      {append, New} ->
             New_next = loop_append( New, Next, My_pid ),
             loop( Data, Previous, New_next );
      {foreach_next, Fun} ->
                catch Fun( Data ),
                loop_foreach_next( Fun, Next ),
                loop( Data, Previous, Next );
      {foreach_previous, Fun} ->
                catch Fun( Data ),
                loop_foreach_previous( Fun, Previous ),
                loop( Data, Previous, Next );
      {free} ->
             ok;
      {insert, New, My_pid} ->
	     New ! {previous, My_pid},
             loop_append( Next, New, My_pid ),
             loop( Data, Previous, New );
      {insert, New, After} ->
             Next ! {insert, New, After},
             loop( Data, Previous, Next );
      {previous, New_previous} ->
             loop( Data, New_previous, Next )
        end.

loop_append( New, nonext, My_pid ) ->
        New ! {previous, My_pid},
        New;
loop_append( New, Next, _My_pid ) ->
        Next ! {append, New},
        Next.

loop_foreach_next( _Fun, nonext ) -> ok;
loop_foreach_next( Fun, Next ) -> Next ! {foreach_next, Fun}.

loop_foreach_previous( _Fun, noprevious ) -> ok;
loop_foreach_previous( Fun, Next ) -> Next ! {foreach_previous, Fun}.
