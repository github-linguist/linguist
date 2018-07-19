priority-queue :-
  TL0 = [3-'Clear drains',
         4-'Feed cat'],

  % we can create a priority queue from a list
  list_to_heap(TL0, Heap0),

  % alternatively we can start from an empty queue
  % get from empty_heap/1.

  % now we add the other elements
  add_to_heap(Heap0, 5, 'Make tea', Heap1),
  add_to_heap(Heap1, 1, 'Solve RC tasks', Heap2),
  add_to_heap(Heap2, 2, 'Tax return', Heap3),

  % we list the content of the heap:
  heap_to_list(Heap3, TL1),
  writeln('Content of the queue'), maplist(writeln, TL1),
  nl,

  % now we retrieve the minimum-priority pair
  get_from_heap(Heap3, Priority, Key, Heap4),
  format('Retrieve top of the queue : Priority ~w, Element ~w~n', [Priority, Key]),
  nl,

  % we list the content of the heap:
  heap_to_list(Heap4, TL2),
  writeln('Content of the queue'), maplist(writeln, TL2).
