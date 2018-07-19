/**
 * Alexander Alvonellos
 */
import java.util.*;
import java.io.*;

public class TPKA {
	public static void main(String... args) {
		double[] input = new double[11];
		double userInput = 0.0;
		Scanner in = new Scanner(System.in);
		for(int i = 0; i < 11; i++) {
			System.out.print("Please enter a number: ");
			String s = in.nextLine();
			try {
				userInput = Double.parseDouble(s);
			} catch (NumberFormatException e) {
				System.out.println("You entered invalid input, exiting");
				System.exit(1);
			}
			input[i] = userInput;
		}
		for(int j = 10; j >= 0; j--) {
			double x = input[j]; double y = f(x);
			if( y < 400.0) {
				System.out.printf("f( %.2f ) = %.2f\n", x, y);
			} else {
				System.out.printf("f( %.2f ) = %s\n", x, "TOO LARGE");
			}
		}
	}

	private static double f(double x) {
		return Math.pow(Math.abs(x), 0.5) + (5*(Math.pow(x, 3)));
	}
}
