DoublyLinkedList.prototype.getTail = function() {
    var tail;
    this.traverse(function(node){tail = node;});
    return tail;
}
DoublyLinkedList.prototype.traverseBackward = function(func) {
    func(this);
    if (this.prev() != null)
        this.prev().traverseBackward(func);
}
DoublyLinkedList.prototype.printBackward = function() {
    this.traverseBackward( function(node) {print(node.value())} );
}

var head = createDoublyLinkedListFromArray([10,20,30,40]);
head.print();
head.getTail().printBackward();
