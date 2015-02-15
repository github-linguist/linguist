import std.array;

class Stack(T) {
    private T[] items;

    @property bool empty() { return items.empty(); }

    void push(T top) { items ~= top; }

    T pop() {
        if (this.empty)
            throw new Exception("Empty Stack.");
        auto top = items.back;
        items.popBack();
        return top;
    }
}

void main() {
    auto s = new Stack!int();
    s.push(10);
    s.push(20);
    assert(s.pop() == 20);
    assert(s.pop() == 10);
    assert(s.empty());
}
