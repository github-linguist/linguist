public class DotProduct {
	
	public static void main(String[] args) {
		double[] a = {1, 3, -5};
		double[] b = {4, -2, -1};
		
		System.out.println(dotProd(a,b));
	}
	
	public static double dotProd(double[] a, double[] b){
		if(a.length != b.length){
			throw new IllegalArgumentException("The dimensions have to be equal!");
		}
		double sum = 0;
		for(int i = 0; i < a.length; i++){
			sum += a[i] * b[i];
		}
		return sum;
	}
}
