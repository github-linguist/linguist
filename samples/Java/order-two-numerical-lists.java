import java.util.Arrays;
import java.util.List;

public class ListOrder{
	public static boolean ordered(double[] first, double[] second){
		if(first.length == 0) return true;
		if(second.length == 0) return false;
		if(first[0] == second[0])
			return ordered(Arrays.copyOfRange(first, 1, first.length),
					Arrays.copyOfRange(second, 1, second.length));
		return first[0] < second[0];
	}
	
	public static <T extends Comparable<? super T>> boolean ordered(List<T> first, List<T> second){
		int i = 0;
		for(; i < first.size() && i < second.size();i++){
			int cmp = first.get(i).compareTo(second.get(i));
			if(cmp == 0) continue;
			if(cmp < 0) return true;
			return false;
		}
		return i == first.size();
	}
	
	public static boolean ordered2(double[] first, double[] second){
		int i = 0;
		for(; i < first.length && i < second.length;i++){
			if(first[i] == second[i]) continue;
			if(first[i] < second[i]) return true;
			return false;
		}
		return i == first.length;
	}
}
