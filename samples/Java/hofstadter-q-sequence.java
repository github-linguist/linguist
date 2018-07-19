import java.util.HashMap;
import java.util.Map;

public class HofQ {
	private static Map<Integer, Integer> q = new HashMap<Integer, Integer>(){{
		put(1, 1);
		put(2, 1);
	}};
	
	private static int[] nUses = new int[100001];//not part of the task
	
	public static int Q(int n){
		nUses[n]++;//not part of the task
		if(q.containsKey(n)){
			return q.get(n);
		}
		int ans = Q(n - Q(n - 1)) + Q(n - Q(n - 2));
		q.put(n, ans);
		return ans;
	}
	
	public static void main(String[] args){
		for(int i = 1; i <= 10; i++){
			System.out.println("Q(" + i + ") = " + Q(i));
		}
		int last = 6;//value for Q(10)
		int count = 0;
		for(int i = 11; i <= 100000; i++){
			int curr = Q(i);
			if(curr < last) count++;
			last = curr;
			if(i == 1000) System.out.println("Q(1000) = " + curr);
		}
		System.out.println("Q(i) is less than Q(i-1) for i <= 100000 " + count + " times");
		
		//Optional stuff below here
		int maxUses = 0, maxN = 0;
		for(int i = 1; i<nUses.length;i++){
			if(nUses[i] > maxUses){
				maxUses = nUses[i];
				maxN = i;
			}
		}
		System.out.println("Q(" + maxN + ") was called the most with " + maxUses + " calls");
	}
}
