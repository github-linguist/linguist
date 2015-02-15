struct SLinkedNode(T) {
    T data;
    typeof(this)* next;
}

void main() {
    alias SLinkedNode!int N;
    N* n = new N(10);
}
