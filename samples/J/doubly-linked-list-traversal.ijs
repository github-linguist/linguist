traverse=:1 :0
  work=. result=. conew 'DoublyLinkedListHead'
  current=. y
  while. y ~: current=. successor__current do.
    work=. (work;result;<u data__current) conew 'DoublyLinkedListElement'
  end.
  result
)
