function BinaryTree(value, left, right) {
    this.value = value;
    this.left = left;
    this.right = right;
}
BinaryTree.prototype.preorder  = function(f) {this.walk(f,['this','left','right'])}
BinaryTree.prototype.inorder   = function(f) {this.walk(f,['left','this','right'])}
BinaryTree.prototype.postorder = function(f) {this.walk(f,['left','right','this'])}
BinaryTree.prototype.walk = function(func, order) {
    for (var i in order)
        switch (order[i]) {
            case "this": func(this.value); break;
            case "left": if (this.left) this.left.walk(func, order); break;
            case "right": if (this.right) this.right.walk(func, order); break;
        }
}
BinaryTree.prototype.levelorder = function(func) {
    var queue = [this];
    while (queue.length != 0) {
        var node = queue.shift();
        func(node.value);
        if (node.left) queue.push(node.left);
        if (node.right) queue.push(node.right);
    }
}

// convenience function for creating a binary tree
function createBinaryTreeFromArray(ary) {
    var left = null, right = null;
    if (ary[1]) left = createBinaryTreeFromArray(ary[1]);
    if (ary[2]) right = createBinaryTreeFromArray(ary[2]);
    return new BinaryTree(ary[0], left, right);
}

var tree = createBinaryTreeFromArray([1, [2, [4, [7]], [5]], [3, [6, [8],[9]]]]);

print("*** preorder ***");   tree.preorder(print);
print("*** inorder ***");    tree.inorder(print);
print("*** postorder ***");  tree.postorder(print);
print("*** levelorder ***"); tree.levelorder(print);
