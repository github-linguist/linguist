def makeQueue() {
  def [var head, var tail] := Ref.promise()

  def writer {
    to enqueue(value) {
      def [nh, nt] := Ref.promise()
      tail.resolve([value, nh])
      tail := nt
    }
  }

  def reader {
    to empty() { return !Ref.isResolved(head) }

    to dequeue(whenEmpty) {
      if (Ref.isResolved(head)) {
        def [value, next] := head
        head := next
        return value
      } else {
        throw.eject(whenEmpty, "pop() of empty queue")
      }
    }
  }

  return [reader, writer]
}
