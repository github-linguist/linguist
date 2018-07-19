import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

public class Yuletide{
	public static void main(String[] args) {
		for(int i = 2008;i<=2121;i++){
			Calendar cal = new GregorianCalendar(i, Calendar.DECEMBER,
					25);
			if(cal.get(Calendar.DAY_OF_WEEK)==Calendar.SUNDAY){
				System.out.println(cal.getTime());
			}
		}
	}
}
