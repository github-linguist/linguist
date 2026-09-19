package cangjie

struct Node<T> where T <: ToString {
    public Node(var value: T,
        let left!: ?Node<T> = None,
        let right!: ?Node<T> = None) {}

    public func traverse(): Unit {
        left?.traverse()
        print(value)
        right?.traverse()
    }

    public mut func set(value: T) {
        this.value = value
    }
}

main() {
    var tree1 = Node(r'X',
        left: Node(r'B', left: Node(r'C', right: Node(r'D'))),
        right: Node(r'E', left: Node(r'F'), right: Node(r'G')))
    tree1.set(r'A')
    tree1.traverse()

    println()
    var tree2 = Node(0,
        left: Node(2, left: Node(3, right: Node(4))),
        right: Node(5, left: Node(6), right: Node(7)))
    tree2.set(1)
    tree2.traverse()
}
