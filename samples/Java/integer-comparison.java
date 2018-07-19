import java.io.*;

public class compInt {
   public static void main(String[] args) {
       try {
           BufferedReader in = new BufferedReader(new InputStreamReader(System.in));

           int nbr1 = Integer.parseInt(in.readLine());
           int nbr2 = Integer.parseInt(in.readLine());

           if(nbr1<nbr2)
               System.out.println(nbr1 + " is less than " + nbr2);

           if(nbr1>nbr2)
                System.out.println(nbr1 + " is greater than " + nbr2);

           if(nbr1==nbr2)
                System.out.println(nbr1 + " is equal to " + nbr2);
       } catch(IOException e) { }
   }
}
