public class VanDerCorput{
	public static double vdc(int n){
		double vdc = 0;
		int denom = 1;
		while(n != 0){
			vdc += n % 2.0 / (denom *= 2);
			n /= 2;
		}
		return vdc;
	}
	
	public static void main(String[] args){
		for(int i = 0; i <= 10; i++){
			System.out.println(vdc(i));
		}
	}
}
