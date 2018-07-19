import java.text.*;
import java.util.*;

public class LastFridays {

    public static void main(String[] args) throws Exception {
        int year = Integer.parseInt(args[0]);
        GregorianCalendar c = new GregorianCalendar(year, 0, 1);

        for (String mon : new DateFormatSymbols(Locale.US).getShortMonths()) {
            if (!mon.isEmpty()) {
                int totalDaysOfMonth = c.getActualMaximum(Calendar.DAY_OF_MONTH);
                c.set(Calendar.DAY_OF_MONTH, totalDaysOfMonth);

                int daysToRollBack = (c.get(Calendar.DAY_OF_WEEK) + 1) % 7;

                int day = totalDaysOfMonth - daysToRollBack;
                c.set(Calendar.DAY_OF_MONTH, day);

                System.out.printf("%d %s %d\n", year, mon, day);

                c.set(year, c.get(Calendar.MONTH) + 1, 1);
            }
        }
    }
}
