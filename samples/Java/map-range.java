public class Range {
	public static void main(String[] args){
		for(float s = 0;s <= 10; s++){
			System.out.println(s + " in [0, 10] maps to "+
					mapRange(0, 10, -1, 0, s)+" in [-1, 0].");
		}
	}
	
	public static double mapRange(double a1, double a2, double b1, double b2, double s){
		return b1 + ((s - a1)*(b2 - b1))/(a2 - a1);
	}
}
