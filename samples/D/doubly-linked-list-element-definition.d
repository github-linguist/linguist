struct Node(T) {
    T data;
    typeof(this)* prev, next;
}

void main() {
    alias N = Node!int;
    N* n = new N(10);
}
