enum SingleLinkedList<T> {
    Node(T, @mut SingleLinkedList<T>),
    None
}
