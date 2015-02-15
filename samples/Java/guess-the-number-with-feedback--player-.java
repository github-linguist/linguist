import java.util.AbstractList;
import java.util.Collections;
import java.util.Scanner;

public class GuessNumber {
    public static final int LOWER = 0, UPPER = 100;
    public static void main(String[] args) {
	System.out.printf("Instructions:\n" +
			  "Think of integer number from %d (inclusive) to %d (exclusive) and\n" +
			  "I will guess it. After each guess, you respond with L, H, or C depending\n" +
			  "on if my guess was too low, too high, or correct.\n",
			  LOWER, UPPER);
	int result = Collections.binarySearch(new AbstractList<Integer>() {
		private final Scanner in = new Scanner(System.in);
		public int size() { return UPPER - LOWER; }
		public Integer get(int i) {
		    System.out.printf("My guess is: %d. Is it too high, too low, or correct? (H/L/C) ", LOWER+i);
		    String s = in.nextLine();
		    assert s.length() > 0;
		    switch (Character.toLowerCase(s.charAt(0))) {
		    case 'l':
			return -1;
		    case 'h':
			return 1;
		    case 'c':
			return 0;
		    }
		    return -1;
		}
	    }, 0);
	if (result < 0)
	    System.out.println("That is impossible.");
	else
	    System.out.printf("Your number is %d.\n", result);
    }
}
