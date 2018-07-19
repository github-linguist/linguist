public class Floyd {
	public static void main(String[] args){
		printTriangle(5);
		printTriangle(14);
	}
	
	private static void printTriangle(int n){
		System.out.println(n + " rows:");
		for(int rowNum = 1, printMe = 1, numsPrinted = 0;
				rowNum <= n; printMe++){
			int cols = (int)Math.ceil(Math.log10(n*(n-1)/2 + numsPrinted + 2));
			System.out.printf("%"+cols+"d ", printMe);
			if(++numsPrinted == rowNum){
				System.out.println();
				rowNum++;
				numsPrinted = 0;
			}
		}
	}
}
