import java.util._
import scala.swing._

def easter(year: Int) = {
  // Start at March 21.
  val cal = new GregorianCalendar(year, 2, 21);

  /*
   * Calculate e = day of Easter, following the 1886 paper,
   * Kalender-Formeln (Calendar Formulae) by Chr. Zeller.
   *   http://www.merlyn.demon.co.uk/zel-1886.htm
   *
   * With Scala, p % 7 gives the wrong result when p < 0. To give the
   * correct result, this code uses "+ 6 * j" where one might have used
   * "- j". This works because because 6 == -1 (mod 7).
   */
  var b = 0
  var d = 0
  if (cal.getTime().before(cal.getGregorianChange())) {
    // Julian calendar
    b = (19 * (year % 19) + 15) % 30
    d = (b + year + year / 4) % 7
  } else {
    // Gregorian calendar
    val j = year / 100
    val a = year % 19
    b = (19 * a + 15 + j - (8 * j + 13) / 25 - j / 4) % 30
    d = (b + year + year / 4 + 6 * j + j / 4 + 2) % 7
    if (d == 0 && (b == 29 || (b == 28 && a > 10))) d = 7
  }
  val e = b + 7 - d  // This counts days after 21 March.

  val df = new java.text.SimpleDateFormat("EEE dd MMM");
  def advance(days: Int) = {
    cal.add(Calendar.DAY_OF_MONTH, days)
    df.format(cal.getTime())
  }

  val ary = new Array[Any](6)
  ary(0) = year
  ary(1) = advance(e)   // Easter
  ary(2) = advance(39)  // Ascension Thursday
  ary(3) = advance(10)  // Pentecost
  ary(4) = advance(7)   // Trinity Sunday
  ary(5) = advance(4)   // Corpus Christi
  ary
}

val columns = Array("AD", "Easter", "Ascension Thursday", "Pentecost",
  "Trinity Sunday", "Corpus Christi")
val rows = (400.to(2000, 100) ++ 2010.to(2020) ++ Array(2100))
  .map(easter(_)).toArray

new MainFrame {
  title = "Holidays related to Easter"
  contents = new ScrollPane {
    contents = new Table(rows, columns)
  }
  size = new java.awt.Dimension(600, 400)
  visible = true
}
