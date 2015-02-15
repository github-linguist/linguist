def executed( prisonerCount:Int, step:Int ) = {

  val prisoners = ((0 until prisonerCount) map (_.toString)).toList

  def behead( dead:Seq[String], alive:Seq[String] )(countOff:Int) : (Seq[String], Seq[String]) = {
    val group = if( alive.size < countOff ) countOff - alive.size else countOff
	
    (dead ++ alive.take(group).drop(group-1), alive.drop(group) ++ alive.take(group-1))
  }

  def beheadN( dead:Seq[String], alive:Seq[String] ) : (Seq[String], Seq[String]) =
    behead(dead,alive)(step)

  def execute( t:(Seq[String], Seq[String]) ) : (Seq[String], Seq[String]) = t._2 match {
    case x :: Nil => (t._1, Seq(x))
    case x :: xs => execute(beheadN(t._1,t._2))
  }

  execute((List(),prisoners))
}

val (dead,alive) = executed(41,3)

println( "Prisoners executed in order:" )
print( dead.mkString(" ") )
	
println( "\n\nJosephus is prisoner " + alive(0) )
