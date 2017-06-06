/*
 * Author: Robert Koeninger
 * License: WTFPL (http://www.wtfpl.net/)
 */

mixin Expr
{
  abstract Obj? eval()
}

class Constant : Expr
{
  Obj? value

  new make(Obj? value) { this.value = value }
  override Obj? eval() { value }
}

enum class Op
{
  plus,
  minus
}

class Infix : Expr
{
  Op op
  Expr left
  Expr right

  new make(Op op, Expr left, Expr right)
  {
    this.op = op
    this.left = left
    this.right = right
  }

  override Obj? eval()
  {
    switch (op)
    {
      case Op.plus:
        return (Int)left.eval() + (Int)right.eval()
      case Op.minus:
        return (Int)left.eval() - (Int)right.eval()
      default:
        throw Err("undefined Op")
    }
  }
}
