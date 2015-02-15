import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;


public class WebTime{
	public static void main(String[] args){
		try{
			URL address = new URL(
					"http://tycho.usno.navy.mil/cgi-bin/timer.pl");
			URLConnection conn = address.openConnection();
			BufferedReader in = new BufferedReader(
					new InputStreamReader(conn.getInputStream()));
			String line;
			while(!(line = in.readLine()).contains("UTC"));
			System.out.println(line.substring(4));
		}catch(IOException e){
			System.err.println("error connecting to server.");
			e.printStackTrace();
		}
	}
}
