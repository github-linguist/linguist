import java.util.HashMap;
public static void main(String[] args){
	String[] keys= {"a", "b", "c"};
	int[] vals= {1, 2, 3};
	HashMap<String, Integer> hash= new HashMap<String, Integer>();

	for(int i= 0; i < keys.length; i++){
	   hash.put(keys[i], vals[i]);
	}
}
