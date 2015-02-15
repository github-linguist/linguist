LinkedList.prototype.insertAfter = function(searchValue, nodeToInsert) {
    if (this._value == searchValue) {
        nodeToInsert.next(this.next());
        this.next(nodeToInsert);
    }
    else if (this.next() == null)
        throw new Error(0, "value '" + searchValue + "' not found in linked list.")
    else
        this.next().insertAfter(searchValue, nodeToInsert);
}
var list = createLinkedListFromArray(['A','B']);
list.insertAfter('A', new LinkedList('C', null));
