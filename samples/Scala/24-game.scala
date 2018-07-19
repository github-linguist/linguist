object TwentyFourGame {
  def main(args: Array[String]) {
    import Parser.TwentyFourParser

    println(welcome)

    var parser = new TwentyFourParser(problemsIterator.next)
    println("Your four digits: "+parser+".")

    var finished = false
    var expressionCount = 1
    do {
      val line = Console.readLine("Expression "+expressionCount+": ")
      line match {
        case "!" =>
          parser = new TwentyFourParser(problemsIterator.next)
          println("New digits: "+parser+".")

        case "q" =>
          finished = true

        case _ =>
          parser readExpression line match {
            case Some(24) => println("That's right!"); finished = true
            case Some(n) => println("Sorry, that's "+n+".")
            case None =>
          }
      }
      expressionCount += 1
    } while (!finished)

    println("Thank you and goodbye!")
  }

  val welcome = """|The 24 Game
                   |
                   |Given any four digits in the range 1 to 9, which may have repetitions,
                   |Using just the +, -, *, and / operators; and the possible use of
                   |brackets, (), show how to make an answer of 24.
                   |
                   |An answer of "q" will quit the game.
                   |An answer of "!" will generate a new set of four digits.
                   |Otherwise you are repeatedly asked for an expression until it evaluates to 24
                   |
                   |Note: you cannot form multiple digit numbers from the supplied digits,
                   |so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.
                   |""".stripMargin

  val problemsIterator = (
    Iterator
    continually List.fill(4)(scala.util.Random.nextInt(9) + 1 toDouble)
    filter hasSolution
  )

  def hasSolution(l: List[Double]) = permute(l) flatMap computeAllOperations exists (_ == 24)

  def computeAllOperations(l: List[Double]): List[Double] = l match {
    case Nil => Nil
    case x :: Nil => l
    case x :: xs =>
      for {
        y <- computeAllOperations(xs)
        z <- if (y == 0) List(x*y, x+y, x-y) else List(x*y, x/y, x+y, x-y)
      } yield z
  }

  def permute(l: List[Double]): List[List[Double]] = l match {
    case Nil => List(Nil)
    case x :: xs =>
      for {
        ys <- permute(xs)
        position <- 0 to ys.length
        (left, right) = ys splitAt position
      } yield left ::: (x :: right)
  }

  object Parser {
    /*  Arithmetic expression grammar production rules in EBNF form:
     *
     * <expr> --> <term> ( '+' <term> | '-' <term> )*
     * <term> --> <factor> ( '*'  <factor> | '/'  <factor> )*
     * <factor> --> '(' <expr> ')' | <digit>
     * <digit> --> 0 | 1  | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
     *
     * Semantically, <digit> can only be a digit from the list of remaining digits.
     */

    class TwentyFourParser(digits: List[Double]) extends scala.util.parsing.combinator.RegexParsers {
      require(digits.length == 4 && digits.forall(d => 0 <= d && d <= 9))
      override val toString = digits.map(_.toInt).mkString(", ")

      // Grammar
      def exprConsumingAllDigits = expr ^? (remainingDigits.allDigitsConsumed, digitsRemainingError) // Guarantees all digits consumed
      def expr : Parser[Double] = term ~ rep( "+" ~ term | "-" ~ term) ^^ solveOperationChain
      def term = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ solveOperationChain
      def factor = "(" ~> expr <~ ")" | digit
      def digit = digitRegex ^? (remainingDigits.consumeDigit, digitNotAllowedError)
      def digitRegex = "\\d".r | digitExpected
      def digitExpected: Parser[String] = ".".r <~ failure(expectedDigitError) // Produces clear error messages

      // Evaluate expressions
      def readExpression(input: String): Option[Double] = {
        remainingDigits = new DigitList(digits) // Initialize list of digits to be consumed
        parseAll(exprConsumingAllDigits, input) match {
          case Success(result, _) => Some(result)
          case NoSuccess(msg, next) =>
            println(ParsingErrorFormatter(msg, next))
            None
        }
      }

      // List of digits to be consumed
      private var remainingDigits: DigitList = _

      // Solve partial results from parsing
      private def solveOperationChain(partialResult: ~[Double,List[~[String,Double]]]): Double = partialResult match {
        case first ~ chain => chain.foldLeft(first)(doOperation)
      }
      private def doOperation(acc: Double, op: ~[String, Double]): Double = op match {
        case "+" ~ operand => acc + operand
        case "-" ~ operand => acc - operand
        case "*" ~ operand => acc * operand
        case "/" ~ operand => acc / operand
        case x => error("Unknown operation "+x+".")
      }

      // Error messages
      private def digitNotAllowedError(d: String) = "Digit "+d+" is not allowed here. Available digits: "+remainingDigits+"."
      private def digitsRemainingError(x: Any) = "Not all digits were consumed. Digits remaining: "+remainingDigits+"."
      private def expectedDigitError = "Unexpected input. Expected a digit from the list: "+remainingDigits+"."
    }

    private object ParsingErrorFormatter {
      def apply[T](msg: String, next: scala.util.parsing.input.Reader[T]) =
        "%s\n%s\n%s\n" format (msg, next.source.toString.trim, " "*(next.offset - 1)+"^")
    }

    private class DigitList(digits: List[Double]) {
      private var remainingDigits = digits
      override def toString = remainingDigits.map(_.toInt).mkString(", ")

      def consumeDigit: PartialFunction[String, Double] = {
        case d if remainingDigits contains d.toDouble =>
          val n = d.toDouble
          remainingDigits = remainingDigits diff List(n)
          n
      }

      def allDigitsConsumed: PartialFunction[Double, Double] = {
        case n if remainingDigits.isEmpty => n
      }
    }
  }
}
