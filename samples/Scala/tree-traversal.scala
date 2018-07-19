case class IntNode(var value: Int, var left: Option[IntNode] = None, var right: Option[IntNode] = None) {

  def preorder[T](f: IntNode => Any): Unit = {
    f(this)
    left.map(_.preorder(f))
    right.map(_.preorder(f))
  }

  def postorder[T](f: IntNode => Any): Unit = {
    left.map(_.postorder(f))
    right.map(_.postorder(f))
    f(this)
  }

  def inorder[T](f: IntNode => Any): Unit = {
    left.map(_.inorder(f))
    f(this)
    right.map(_.inorder(f))
  }

  def levelorder[T](f: IntNode => Any): Unit = {

    def loVisit(ls: List[IntNode]): Unit = ls match {
      case Nil => None
      case h :: rest => f(h); loVisit(rest ++ h.left ++ h.right)
    }

    loVisit(List(this))
  }
}

object TreeTraversal extends App {
  implicit def intNode2SomeIntNode(n: IntNode) = Some[IntNode](n)

  val tree = IntNode(1,
    IntNode(2,
      IntNode(4,
        IntNode(7)),
      IntNode(5)),
    IntNode(3,
      IntNode(6,
        IntNode(8),
        IntNode(9))))

  List(
    "preorder" -> tree.preorder _,
    "inorder" -> tree.inorder _,
    "postorder" -> tree.postorder _,
    "levelorder" -> tree.levelorder _) foreach {
      case (name, func) =>
        var s = new StringBuilder("%10s: ".format(name))
        func(n => s ++= n.value.toString + " ")
        println(s)
    }
}
