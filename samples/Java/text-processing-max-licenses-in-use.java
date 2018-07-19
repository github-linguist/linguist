import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedList;

public class License {
  public static void main(String[] args) throws FileNotFoundException, IOException{
    BufferedReader in = new BufferedReader(new FileReader(args[0]));
    int max = Integer.MIN_VALUE;
    LinkedList<String> dates = new LinkedList<String>();
    String line;
    int count = 0;
    while((line = in.readLine()) != null){
      if(line.startsWith("License OUT ")) count++;
      if(line.startsWith("License IN ")) count--;
      if(count > max){
        max = count;
        String date = line.split(" ")[3];
        dates.clear();
        dates.add(date);
      }else if(count == max){
        String date = line.split(" ")[3];
        dates.add(date);
      }
    }
    System.out.println("Max licenses out: "+max);
    System.out.println("At time(s): "+dates);
  }
}
