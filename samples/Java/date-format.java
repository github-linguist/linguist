import java.util.Calendar;
import java.util.GregorianCalendar;
import java.text.DateFormatSymbols;
import java.util.Date;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
public class Dates
{
 public static void main(final String[] args)
 {
  Calendar now = new GregorianCalendar(); //months are 0 indexed, dates are 1 indexed
  DateFormatSymbols symbols = new DateFormatSymbols(); //names for our months and weekdays

  //plain numbers way
  System.out.println(now.get(Calendar.YEAR)  + "-" + (now.get(Calendar.MONTH) + 1) + "-" + now.get(Calendar.DATE));

  //words way
  System.out.print(symbols.getWeekdays()[now.get(Calendar.DAY_OF_WEEK)] + ", ");
  System.out.print(symbols.getMonths()[now.get(Calendar.MONTH)] + " ");
  System.out.println(now.get(Calendar.DATE) + ", " + now.get(Calendar.YEAR));

  //using DateFormat
  Date date = new Date();
  DateFormat format1 = new SimpleDateFormat("yyyy-MM-dd");
  System.out.println(format1.format(date));
  DateFormat format2 = new SimpleDateFormat("EEEE, MMMM dd, yyyy");
  System.out.println(format2.format(date));
 }
}
