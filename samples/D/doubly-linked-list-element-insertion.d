import std.stdio;

struct Node(T) {
    T data;
    typeof(this)* prev, next;
}

/// If prev is null, prev gets to point to a new node.
void insertAfter(T)(ref Node!T* prev, T item) pure nothrow {
    if (prev) {
        auto newNode = new Node!T(item, prev, prev.next);
        prev.next = newNode;
        if (newNode.next)
            newNode.next.prev = newNode;
    } else
        prev = new Node!T(item);
}

void show(T)(Node!T* list) {
    while (list) {
        write(list.data, " ");
        list = list.next;
    }
    writeln;
}

void main() {
    Node!(string)* list;
    insertAfter(list, "A");
    list.show;
    insertAfter(list, "B");
    list.show;
    insertAfter(list, "C");
    list.show;
}
