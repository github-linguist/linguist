def trimLeft(str: String) = str dropWhile(_.isWhitespace)

def trimRight(str: String) = str take (str.lastIndexWhere(!_.isWhitespace) + 1)

def trimRight2(str: String) = trimLeft(str reverse) reverse

def trim(str: String) = str trim
		
def testTrim() = {
  val str = "  \u001F  String with spaces \t  \n  \r "
  println("original  : |" + str + "|")
  println("trimLeft  : |" + trimLeft(str) + "|")
  println("trimRight : |" + trimRight(str) + "|")
  println("trimRight2: |" + trimRight2(str) + "|")
  println("trim      : |" + trim(str) + "|")
}
