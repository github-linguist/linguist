sealed trait Trit { self =>
  def nand(that:Trit):Trit=(this,that) match {
    case (TFalse, _) => TTrue
    case (_, TFalse) => TTrue
    case (TMaybe, _) => TMaybe
    case (_, TMaybe) => TMaybe
    case _ => TFalse
  }
		
  def nor(that:Trit):Trit = this.or(that).not()
  def and(that:Trit):Trit = this.nand(that).not()
  def or(that:Trit):Trit = this.not().nand(that.not())
  def not():Trit = this.nand(this)
  def imply(that:Trit):Trit = this.nand(that.not())
  def equiv(that:Trit):Trit = this.and(that).or(this.nor(that))
}
case object TTrue extends Trit
case object TMaybe extends Trit
case object TFalse extends Trit

object TernaryLogic extends App {
  val v=List(TTrue, TMaybe, TFalse)
  println("- NOT -")
  for(a<-v) println("%6s => %6s".format(a, a.not))
  println("\n- AND -")
  for(a<-v; b<-v) println("%6s : %6s => %6s".format(a, b, a and b))
  println("\n- OR -")
  for(a<-v; b<-v) println("%6s : %6s => %6s".format(a, b, a or b))
  println("\n- Imply -")
  for(a<-v; b<-v) println("%6s : %6s => %6s".format(a, b, a imply b))
  println("\n- Equiv -")
  for(a<-v; b<-v) println("%6s : %6s => %6s".format(a, b, a equiv b))		
}
