import java.text.SimpleDateFormat
import java.util.{Calendar, Locale, TimeZone}

object DateManipulation {
  def main(args: Array[String]): Unit = {
    val input="March 7 2009 7:30pm EST"
    val df=new SimpleDateFormat("MMMM d yyyy h:mma z", Locale.ENGLISH)
    val c=Calendar.getInstance()
    c.setTime(df.parse(input))
	
    c.add(Calendar.HOUR_OF_DAY, 12)
    println(df.format(c.getTime))
	
    df.setTimeZone(TimeZone.getTimeZone("GMT"))
    println(df.format(c.getTime))
  }
}
