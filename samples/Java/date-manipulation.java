import java.util.Date;
import java.text.SimpleDateFormat;
public class DateManip{
    public static void main(String[] args) throws Exception{
	String dateStr = "March 7 2009 7:30pm EST";

	SimpleDateFormat sdf = new SimpleDateFormat("MMMM d yyyy h:mma zzz");
	
	Date date = sdf.parse(dateStr);
	
	date.setTime(date.getTime() + 43200000l);

	System.out.println(sdf.format(date));
    }

}
