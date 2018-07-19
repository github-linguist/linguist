package org.rosetta.arithmetic_evaluator.scala

object ArithmeticParser extends scala.util.parsing.combinator.RegexParsers {

  def readExpression(input: String) : Option[()=>Int] = {
    parseAll(expr, input) match {
      case Success(result, _) =>
        Some(result)
      case other =>
        println(other)
        None
    }
  }

  private def expr : Parser[()=>Int] = {
    (term<~"+")~expr ^^ { case l~r => () => l() + r() } |
    (term<~"-")~expr ^^ { case l~r => () => l() - r() } |
    term
  }

  private def term : Parser[()=>Int] = {
    (factor<~"*")~term ^^ { case l~r => () => l() * r() } |
    (factor<~"/")~term ^^ { case l~r => () => l() / r() } |
    factor
  }

  private def factor : Parser[()=>Int] = {
    "("~>expr<~")" |
    "\\d+".r ^^ { x => () => x.toInt } |
    failure("Expected a value")
  }
}

object Main {
  def main(args: Array[String]) {
    println("""Please input the expressions. Type "q" to quit.""")
    var input: String = ""

    do {
      input = readLine("> ")
      if (input != "q") {
        ArithmeticParser.readExpression(input).foreach(f => println(f()))
      }
    } while (input != "q")
  }
}
