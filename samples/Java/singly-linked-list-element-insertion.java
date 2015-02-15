void insertNode(Node<T> anchor_node, Node<T> new_node)
{
    new_node.next = anchor_node.next;
    anchor_node.next = new_node;
}
