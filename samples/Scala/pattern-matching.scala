class RedBlackTree[A](implicit ord: Ordering[A]) {
  sealed abstract class Color
  case object R extends Color
  case object B extends Color

  sealed abstract class Tree {
    def insert(x: A): Tree = ins(x) match {
      case T(_, a, y, b) => T(B, a, y, b)
      case E             => E
    }
    def ins(x: A): Tree
  }

  case object E extends Tree {
    override def ins(x: A): Tree = T(R, E, x, E)
  }

  case class T(c: Color, left: Tree, a: A, right: Tree) extends Tree {
    private def balance: Tree = (c, left, a, right) match {
      case (B, T(R, T(R, a, x, b), y, c),             z, d                                    ) => T(R, T(B, a, x, b), y, T(B, c, z, d))
      case (B, T(R, a,             x, T(R, b, y, c)), z, d                                    ) => T(R, T(B, a, x, b), y, T(B, c, z, d))
      case (B, a,                                     x, T(R, T(R, b, y, c), z, d            )) => T(R, T(B, a, x, b), y, T(B, c, z, d))
      case (B, a,                                     x, T(R, b,             y, T(R, c, z, d))) => T(R, T(B, a, x, b), y, T(B, c, z, d))
      case _ => this
    }

    override def ins(x: A): Tree = ord.compare(x, a) match {
      case -1 => T(c, left ins x, a, right      ).balance
      case  1 => T(c, left,       a, right ins x).balance
      case  0 => this
    }
  }
}
