function DoublyLinkedList(value, next, prev) {
    this._value = value;
    this._next = next;
    this._prev = prev;
}
// from LinkedList, inherit: value(), next(), traverse(), print()
DoublyLinkedList.prototype = new LinkedList();

DoublyLinkedList.prototype.prev = function() {
    if (arguments.length == 1)
        this._prev = arguments[0];
    else
        return this._prev;
}

function createDoublyLinkedListFromArray(ary) {
    var node, prev, head = new DoublyLinkedList(ary[0], null, null);
    prev = head;
    for (var i = 1; i < ary.length; i++) {
        node = new DoublyLinkedList(ary[i], null, prev);
        prev.next(node);
        prev = node;
    }
    return head;
}

var head = createDoublyLinkedListFromArray([10,20,30,40]);
