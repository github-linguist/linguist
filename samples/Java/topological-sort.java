import java.util.ArrayList;
import java.util.TreeMap;

public class TopologicalSort {
	public static void main(String[] args) throws Exception {
		TreeMap<String, ArrayList<String>> mp = new TreeMap<String, ArrayList<String>>();
		String[] data, input = new String[] {
				"des_system_lib: std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee",
				"dw01: ieee dw01 dware gtech", "dw02: ieee dw02 dware",
				"dw03: std synopsys dware dw03 dw02 dw01 ieee gtech",
				"dw04: dw04 ieee dw01 dware gtech", "dw05: dw05 ieee dware",
				"dw06: dw06 ieee dware", "dw07: ieee dware",
				"dware: ieee dware", "gtech: ieee gtech", "ramlib: std ieee",
				"std_cell_lib: ieee std_cell_lib", "synopsys:" };

		for (String str : input)
			mp.put((data = str.split(":"))[0], Utils.aList(//
					data.length < 2 || data[1].trim().equals("")//
					? null : data[1].trim().split("\\s+")));

		Utils.tSortFix(mp);
		System.out.println(Utils.tSort(mp));
		mp.put("dw01", Utils.aList("dw04"));
		System.out.println(Utils.tSort(mp));
	}
}
