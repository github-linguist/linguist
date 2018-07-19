public class IdentityMatrix {

	public static int[][] matrix(int n){
		int[][] array = new int[n][n];
		
		for(int row=0; row<n; row++){
			for(int col=0; col<n; col++){
				if(row == col){
					array[row][col] = 1;
				}
				else{
					array[row][col] = 0;
				}
			}
		}
		return array;
	}
	public static void printMatrix(int[][] array){
		for(int row=0; row<array.length; row++){
			for(int col=0; col<array[row].length; col++){
				System.out.print(array[row][col] + "\t");
			}
			System.out.println();
		}
	}
	public static void main (String []args){
		printMatrix(matrix(5));
	}
}
By Sani Yusuf @saniyusuf.
