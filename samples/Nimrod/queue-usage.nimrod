import queues

var deq: TQueue[int] = initQueue[int]()

deq.enqueue(26)
deq.add(99)     # same as enqueue()
deq.enqueue(2)
echo("Dequeue size: ", deq.len())
echo("De-queue: ", deq.dequeue())
echo("De-queue: ", deq.dequeue())
echo("De-queue: ", deq.dequeue())
#echo("De-queue: ", deq.dequeue())     # dequeue an empty dequeue raises [EAssertionFailed]
