class TreeNode<T> {

  T value;
  TreeNode<T> left;
  TreeNode<T> right;

  TreeNode(this.value);

  TreeNode map(T f(T t)) {
    var node = new TreeNode(f(value));
    if(left != null) {
      node.left = left.map(f);
    }
    if(right != null) {
      node.right = right.map(f);
    }
    return node;
  }

  void forEach(void f(T t)) {
    f(value);
    if(left != null) {
      left.forEach(f);
    }
    if(right != null) {
      right.forEach(f);
    }
  }
}

void main() {
  TreeNode root = new TreeNode(1);
  root.left = new TreeNode(2);
  root.right = new TreeNode(3);
  root.left.right = new TreeNode(4);

  print('first tree');
  root.forEach(print);
  var newRoot = root.map((t) => t * 222);
  print('second tree');
  newRoot.forEach(print);
}
