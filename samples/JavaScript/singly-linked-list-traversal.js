LinkedList.prototype.traverse = function(func) {
    func(this);
    if (this.next() != null)
        this.next().traverse(func);
}

LinkedList.prototype.print = function() {
    this.traverse( function(node) {print(node.value())} );
}

var head = createLinkedListFromArray([10,20,30,40]);
head.print();
