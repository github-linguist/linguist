struct SLinkedNode(T) {
    T data;
    typeof(this)* next;
}

void insertAfter(T)(SLinkedNode!T* listNode, SLinkedNode!T* newNode) {
    newNode.next = listNode.next;
    listNode.next = newNode;
}

void main() {
    alias N = SLinkedNode!char;

    auto lh = new N('A', new N('B'));
    auto c = new N('C');

    // Inserts C after A, creating the (A C B) list:
    insertAfter(lh, c);

    // The GC will collect the memory.
}
