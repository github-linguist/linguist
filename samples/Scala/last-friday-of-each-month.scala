import java.util.Calendar
import java.text.SimpleDateFormat

object Fridays {

  def lastFridayOfMonth(year:Int, month:Int)={
    val cal=Calendar.getInstance
    cal.set(Calendar.YEAR, year)
    cal.set(Calendar.MONTH, month)
    cal.set(Calendar.DAY_OF_WEEK, Calendar.FRIDAY)
    cal.set(Calendar.DAY_OF_WEEK_IN_MONTH, -1)
    cal.getTime
  }
	
  def fridaysOfYear(year:Int)=for(month <- 0 to 11) yield lastFridayOfMonth(year, month)
	
  def main(args:Array[String]){
    val year=args(0).toInt
    val formatter=new SimpleDateFormat("yyyy-MMM-dd")
    fridaysOfYear(year).foreach{date=>
      println(formatter.format(date))
    }
  }
}
