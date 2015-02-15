import java.util.Arrays;
import java.util.Random;

public class OneOfNLines {

	static Random rand;
	
	public static int oneOfN(int n) {
		int choice = 0;
		
		for(int i = 1; i < n; i++) {
			if(rand.nextInt(i+1) == 0)
				choice = i;
		}
		
		return choice;
	}
	
	public static void main(String[] args) {
		int n = 10;
		int trials = 1000000;
		int[] bins = new int[n];
		rand = new Random();
		
		for(int i = 0; i < trials; i++)
			bins[oneOfN(n)]++;
		
		
		System.out.println(Arrays.toString(bins));
	}
}
