import java.util.{Date, TimeZone, Locale}
import java.text.DateFormat

val df=DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG, Locale.ENGLISH)
df.setTimeZone(TimeZone.getTimeZone("UTC"))
println(df.format(new Date(0)))
