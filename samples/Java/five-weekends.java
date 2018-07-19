import java.util.Calendar;
import java.util.GregorianCalendar;

public class FiveFSS {
    private static boolean[] years = new boolean[201];
    //dreizig tage habt september...
    private static int[] month31 = {Calendar.JANUARY, Calendar.MARCH, Calendar.MAY,
        Calendar.JULY, Calendar.AUGUST, Calendar.OCTOBER, Calendar.DECEMBER};

    public static void main(String[] args) {
        StringBuilder months = new StringBuilder();
        int numMonths = 0;
        for (int year = 1900; year <= 2100; year++) {
            for (int month : month31) {
                Calendar date = new GregorianCalendar(year, month, 1);
                if (date.get(Calendar.DAY_OF_WEEK) == Calendar.FRIDAY) {
                    years[year - 1900] = true;
                    numMonths++;
                    //months are 0-indexed in Calendar
                    months.append((date.get(Calendar.MONTH) + 1) + "-" + year +"\n");
                }
            }
        }
        System.out.println("There are "+numMonths+" months with five weekends from 1900 through 2100:");
        System.out.println(months);
        System.out.println("Years with no five-weekend months:");
        for (int year = 1900; year <= 2100; year++) {
            if(!years[year - 1900]){
                System.out.println(year);
            }
        }
    }
}
