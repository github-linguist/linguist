DoublyLinkedList.prototype.insertAfter = function(searchValue, nodeToInsert) {
    if (this._value == searchValue) {
        var after = this.next();
        this.next(nodeToInsert);
        nodeToInsert.prev(this);
        nodeToInsert.next(after);
        after.prev(nodeToInsert);
    }
    else if (this.next() == null)
        throw new Error(0, "value '" + searchValue + "' not found in linked list.")
    else
        this.next().insertAfter(searchValue, nodeToInsert);
}

var list = createDoublyLinkedListFromArray(['A','B']);
list.insertAfter('A', new DoublyLinkedList('C', null, null));
