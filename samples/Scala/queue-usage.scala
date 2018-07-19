val q=scala.collection.mutable.Queue[Int]()
println("isEmpty = " + q.isEmpty)
try{q dequeue} catch{case _:java.util.NoSuchElementException => println("dequeue(empty) failed.")}
q enqueue 1
q enqueue 2
q enqueue 3
println("queue   = " + q)
println("front   = " + q.front)
println("dequeue = " + q.dequeue)
println("dequeue = " + q.dequeue)
println("isEmpty = " + q.isEmpty)
