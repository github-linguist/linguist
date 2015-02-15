object Node {
  def insert(a: Node, c: Node) = {
    c.next = a.next
    a.next = c
  }
}
