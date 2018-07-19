import java.io.*;
import java.util.*;

public class DerangedAnagrams {

    public static void main(final String[] args) throws IOException {
        if (!findAnagrams(readLines("unixdict.txt")))
            System.out.println("no result");
    }

    private static boolean isDeranged(final String w, final List<String> lst) {
        for (String w2 : lst) {
            int k = w.length() - 1;
            while (k >= 0 && w.charAt(k) != w2.charAt(k)) {
                k--;
            }
            if (k == -1) {
                System.out.println(w + ", " + w2);
                return true;
            }
        }
        return false;
    }

    private static boolean findAnagrams(final List<String> words) {
        Collections.sort(words, new Comparator<String>() {
            public int compare(String a, String b) {
                return b.length() - a.length();
            }
        });
        Map<String, ArrayList<String>> map = new HashMap<>();
        for (String w : words) {
            char[] srt = w.toCharArray();
            Arrays.sort(srt);
            String key = String.valueOf(srt);
            ArrayList<String> lst;
            if (map.containsKey(key)) {
                lst = map.get(key);
                if (isDeranged(w, lst)) {
                    return true;
                }
                lst.add(w);
            } else {
                lst = new ArrayList<>();
                lst.add(w);
                map.put(key, lst);
            }
        }
        return false;
    }

    private static List<String> readLines(final String fn) throws IOException {
        List<String> lines;
        try (BufferedReader br = new BufferedReader(new FileReader(fn))) {
            lines = new ArrayList<>();
            String line = null;
            while ((line = br.readLine()) != null)
                lines.add(line);
        }
        return lines;
    }
}
