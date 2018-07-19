import java.util.Scanner;

public class LCM{
   public static void main(String[] args){
      Scanner aScanner = new Scanner(System.in);

      //prompts user for values to find the LCM for, then saves them to m and n
      System.out.print("Enter the value of m:");
      int m = aScanner.nextInt();
      System.out.print("Enter the value of n:");
      int n = aScanner.nextInt();
      int lcm = (n == m || n == 1) ? m :(m == 1 ? n : 0);
      /* this section increases the value of mm until it is greater
      / than or equal to nn, then does it again when the lesser
      / becomes the greater--if they aren't equal. If either value is 1,
      / no need to calculate*/
      if (lcm == 0) {
         int mm = m, nn = n;
         while (mm != nn) {
             while (mm < nn) { mm += m; }
             while (nn < mm) { nn += n; }
         }
         lcm = mm;
      }
      System.out.println("lcm(" + m + ", " + n + ") = " + lcm);
   }
}
