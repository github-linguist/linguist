import java.text.DateFormat;
import java.util.Date;
import java.util.TimeZone;

public class DateTest{
    public static void main(String[] args) {
        Date date = new Date(0);
        DateFormat format = DateFormat.getDateTimeInstance();
        format.setTimeZone(TimeZone.getTimeZone("UTC"));
        System.out.println(format.format(date));
    }
}
