public class Equlibrium {
	public static void main(String[] args) {
		int[] sequence = {-7, 1, 5, 2, -4, 3, 0};
		equlibrium_indices(sequence);
	}

	public static void equlibrium_indices(int[] sequence){
		//find total sum
		int totalSum = 0;
		for (int n : sequence) {
			totalSum += n;
		}
		//compare running sum to remaining sum to find equlibrium indices
		int runningSum = 0;
		for (int i = 0; i < sequence.length; i++) {
			int n = sequence[i];
			if (totalSum - runningSum - n == runningSum) {
				System.out.println(i);
			}
			runningSum += n;
		}
	}
}
