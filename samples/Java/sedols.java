import java.util.Scanner;

public class SEDOL{
	public static void main(String[] args){
		Scanner sc = new Scanner(System.in);
		while(sc.hasNext()){
			String sedol = sc.next();
			System.out.println(sedol + getSedolCheckDigit(sedol));
		}
	}
	
	private static final int[] mult = {1, 3, 1, 7, 3, 9};
	
	public static int getSedolCheckDigit(String str){
	    if(!validateSedol(str)){
	    	System.err.println("SEDOL strings must contain six characters with no vowels.");
	    	return -1;
	    }
	    str = str.toUpperCase();
	    int total = 0;
	    for(int i = 0;i < 6; i++){
	        char s = str.charAt(i);
	        total += Character.digit(s, 36) * mult[i];
	    }
	    return (10 - (total % 10)) % 10;
	}

	public static boolean validateSedol(String str){
		return (str.length() == 6) && !str.toUpperCase().matches(".*?[AEIOU].*?");
	}
}
