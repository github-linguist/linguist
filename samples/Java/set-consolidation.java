import java.util.*;

public class SetConsolidation {

    public static void main(String[] args) {
        List<Set<Character>> h1 = hashSetList("AB", "CD");
        System.out.println(consolidate(h1));

        List<Set<Character>> h2 = hashSetList("AB", "BD");
        System.out.println(consolidateR(h2));

        List<Set<Character>> h3 = hashSetList("AB", "CD", "DB");
        System.out.println(consolidate(h3));

        List<Set<Character>> h4 = hashSetList("HIK", "AB", "CD", "DB", "FGH");
        System.out.println(consolidateR(h4));
    }

    // iterative
    private static <E> List<Set<E>>
                consolidate(Collection<? extends Set<E>> sets) {
	List<Set<E>> r = new ArrayList<>();
	for (Set<E> s : sets) {
	    List<Set<E>> new_r = new ArrayList<>();
	    new_r.add(s);
	    for (Set<E> x : r) {
		if (!Collections.disjoint(s, x)) {
		    s.addAll(x);
		} else {
		    new_r.add(x);
		}
	    }
	    r = new_r;
	}
	return r;
    }

    // recursive
    private static <E> List<Set<E>> consolidateR(List<Set<E>> sets) {
        if (sets.size() < 2)
            return sets;
        List<Set<E>> r = new ArrayList<>();
        r.add(sets.get(0));
        for (Set<E> x : consolidateR(sets.subList(1, sets.size()))) {
            if (!Collections.disjoint(r.get(0), x)) {
                r.get(0).addAll(x);
            } else {
                r.add(x);
            }
        }
        return r;
    }

    private static List<Set<Character>> hashSetList(String... set) {
        List<Set<Character>> r = new ArrayList<>();
        for (int i = 0; i < set.length; i++) {
            r.add(new HashSet<Character>());
            for (int j = 0; j < set[i].length(); j++)
                r.get(i).add(set[i].charAt(j));
        }
        return r;
    }
}
