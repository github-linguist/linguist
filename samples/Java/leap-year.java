import java.util.GregorianCalendar;
import java.text.MessageFormat;

public class Leapyear {
        public static void main(String[] argv) {
                int[] yrs = {1800,1900,1994,1998,1999,2000,2001,2004,2100};
                GregorianCalendar cal = new GregorianCalendar();
                for (int year : yrs) {
                        System.err.println(MessageFormat.format("The year {0,number,#} is leaper: {1} / {2}.", year, cal.isLeapYear(year), is_leapyear(year)));
                }

        }
        public static boolean is_leapyear(int year) {
                return (year % 100 == 0) ? (year % 400 == 0) : (year % 4 == 0);
        }
}
