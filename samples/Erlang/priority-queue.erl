-module( priority_queue ).

-export( [create/0, insert/3, peek/1, task/0, top/1] ).

create() -> gb_trees:empty().

insert( Element, Priority, Queue ) -> gb_trees:enter( Priority, Element, Queue ).

peek( Queue ) ->
  {_Priority, Element, _New_queue} = gb_trees:take_smallest( Queue ),
  Element.

task() ->
  Items = [{3, "Clear drains"}, {4, "Feed cat"}, {5, "Make tea"}, {1, "Solve RC tasks"}, {2, "Tax return"}],
  Queue = lists:foldl( fun({Priority, Element}, Acc) -> insert( Element, Priority, Acc ) end, create(), Items ),
  io:fwrite( "peek priority: ~p~n", [peek( Queue )] ),
  lists:foldl( fun(_N, Q) -> write_top( Q ) end, Queue, lists:seq(1, erlang:length(Items)) ).

top( Queue ) ->
  {_Priority, Element, New_queue} = gb_trees:take_smallest( Queue ),
  {Element, New_queue}.



write_top( Q ) ->
  {Element, New_queue} = top( Q ),
  io:fwrite( "top priority: ~p~n", [Element] ),
  New_queue.
