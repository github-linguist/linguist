import java.util.Scanner;
public class Int{
  public static void main(String[] args){
    Scanner sc = new Scanner(System.in);
    int a = sc.nextInt();
    int b = sc.nextInt();

    int sum = a + b;//integer addition is discouraged in print statements due to confusion with String concatenation
    System.out.println("a + b = " + sum);
    System.out.println("a - b = " + (a - b));
    System.out.println("a * b = " + (a * b));
    System.out.println("quotient of a / b = " + (a / b)); // truncates towards 0
    System.out.println("remainder of a / b = " + (a % b)); // same sign as first operand
  }
}
