import java.util.HashMap;
import java.util.Map;

public class Catalan {
	private static final Map<Long, Double> facts = new HashMap<Long, Double>();
	private static final Map<Long, Double> catsI = new HashMap<Long, Double>();
	private static final Map<Long, Double> catsR1 = new HashMap<Long, Double>();
	private static final Map<Long, Double> catsR2 = new HashMap<Long, Double>();
	
	static{//pre-load the memoization maps with some answers
		facts.put(0L, 1D);
		facts.put(1L, 1D);
		facts.put(2L, 2D);
		
		catsI.put(0L, 1D);
		catsR1.put(0L, 1D);
		catsR2.put(0L, 1D);
	}

	private static double fact(long n){
		if(facts.containsKey(n)){
			return facts.get(n);
		}
		double fact = 1;
		for(long i = 2; i <= n; i++){
			fact *= i; //could be further optimized, but it would probably be ugly
		}
		facts.put(n, fact);
		return fact;
	}

	private static double catI(long n){
		if(!catsI.containsKey(n)){
			catsI.put(n, fact(2 * n)/(fact(n+1)*fact(n)));
		}
		return catsI.get(n);
	}
	
	private static double catR1(long n){
		if(catsR1.containsKey(n)){
			return catsR1.get(n);
		}
		double sum = 0;
		for(int i = 0; i < n; i++){
			sum += catR1(i) * catR1(n - 1 - i);
		}
		catsR1.put(n, sum);
		return sum;
	}
	
	private static double catR2(long n){
		if(!catsR2.containsKey(n)){
			catsR2.put(n, ((2.0*(2*(n-1) + 1))/(n + 1)) * catR2(n-1));
		}
		return catsR2.get(n);
	}
	
	public static void main(String[] args){
		for(int i = 0; i <= 15; i++){
			System.out.println(catI(i));
			System.out.println(catR1(i));
			System.out.println(catR2(i));
		}
	}
}
