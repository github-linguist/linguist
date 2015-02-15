import queues

# defining push & pop (obviously optional)
proc push*[T](q: var TQueue[T]; item: T) =
    add(q,item)
proc pop*[T](q: var TQueue[T]): T =
    result = dequeue(q)

var fifo: TQueue[int] = initQueue[int]()

fifo.push(26)
fifo.push(99)
fifo.push(2)
echo("Fifo size: ", fifo.len())
echo("Popping: ", fifo.pop())
echo("Popping: ", fifo.pop())
echo("Popping: ", fifo.pop())
#echo("Popping: ", fifo.pop())     # popping an empty stack raises [EAssertionFailed]
