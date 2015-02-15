class Tree
{
  readonly Int label
  readonly Tree? left
  readonly Tree? right

  new make (Int label, Tree? left := null, Tree? right := null)
  {
    this.label = label
    this.left = left
    this.right = right
  }

  Void preorder(|Int->Void| func)
  {
    func(label)
    left?.preorder(func) // ?. will not call method if 'left' is null
    right?.preorder(func)
  }

  Void postorder(|Int->Void| func)
  {
    left?.postorder(func)
    right?.postorder(func)
    func(label)
  }

  Void inorder(|Int->Void| func)
  {
    left?.inorder(func)
    func(label)
    right?.inorder(func)
  }

  Void levelorder(|Int->Void| func)
  {
    Tree[] nodes := [this]
    while (nodes.size > 0)
    {
      Tree cur := nodes.removeAt(0)
      func(cur.label)
      if (cur.left != null) nodes.add (cur.left)
      if (cur.right != null) nodes.add (cur.right)
    }
  }
}

class Main
{
  public static Void main ()
  {
    tree := Tree(1,
              Tree(2, Tree(4, Tree(7)), Tree(5)),
              Tree(3, Tree(6, Tree(8), Tree(9))))
    List result := [,]
    collect := |Int a -> Void| { result.add(a) }
    tree.preorder(collect)
    echo ("preorder:    " + result.join(" "))
    result = [,]
    tree.inorder(collect)
    echo ("inorder:     " + result.join(" "))
    result = [,]
    tree.postorder(collect)
    echo ("postorder:   " + result.join(" "))
    result = [,]
    tree.levelorder(collect)
    echo ("levelorder:  " + result.join(" "))
  }
}
