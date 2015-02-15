import org.joda.time.*
import java.text.*

def dateString = 'March 7 2009 7:30pm EST'

def sdf = new SimpleDateFormat('MMMM d yyyy h:mma zzz')

DateTime dt = new DateTime(sdf.parse(dateString))

println (dt)
println (dt.plusHours(12))
println (dt.plusHours(12).withZone(DateTimeZone.UTC))
