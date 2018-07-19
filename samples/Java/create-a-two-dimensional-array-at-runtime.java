import java.util.Scanner;

public class twoDimArray {
  public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        int nbr1 = in.nextInt();
        int nbr2 = in.nextInt();

        double[][] array = new double[nbr1][nbr2];
        array[0][0] = 42.0;
        System.out.println("The number at place [0 0] is " + array[0][0]);
  }
}
