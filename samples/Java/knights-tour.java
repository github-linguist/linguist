import java.util.*;

public class KnightsTour {
    private final static int base = 12;
    private final static int[][] moves = {{1,-2},{2,-1},{2,1},{1,2},{-1,2},
        {-2,1},{-2,-1},{-1,-2}};
    private static int[][] grid;
    private static int total;

    public static void main(String[] args) {
        grid = new int[base][base];
        total = (base - 4) * (base - 4);

        for (int r = 0; r < base; r++)
            for (int c = 0; c < base; c++)
                if (r < 2 || r > base - 3 || c < 2 || c > base - 3)
                    grid[r][c] = -1;

        int row = 2 + (int) (Math.random() * (base - 4));
        int col = 2 + (int) (Math.random() * (base - 4));

        grid[row][col] = 1;

        if (solve(row, col, 2))
            printResult();
        else System.out.println("no result");

    }

    private static boolean solve(int r, int c, int count) {
        if (count > total)
            return true;

        List<int[]> nbrs = neighbors(r, c);

        if (nbrs.isEmpty() && count != total)
            return false;

        Collections.sort(nbrs, new Comparator<int[]>() {
            public int compare(int[] a, int[] b) {
                return a[2] - b[2];
            }
        });

        for (int[] nb : nbrs) {
            r = nb[0];
            c = nb[1];
            grid[r][c] = count;
            if (!orphanDetected(count, r, c) && solve(r, c, count + 1))
                return true;
            grid[r][c] = 0;
        }

        return false;
    }

    private static List<int[]> neighbors(int r, int c) {
        List<int[]> nbrs = new ArrayList<>();

        for (int[] m : moves) {
            int x = m[0];
            int y = m[1];
            if (grid[r + y][c + x] == 0) {
                int num = countNeighbors(r + y, c + x);
                nbrs.add(new int[]{r + y, c + x, num});
            }
        }
        return nbrs;
    }

    private static int countNeighbors(int r, int c) {
        int num = 0;
        for (int[] m : moves)
            if (grid[r + m[1]][c + m[0]] == 0)
                num++;
        return num;
    }

    private static boolean orphanDetected(int cnt, int r, int c) {
        if (cnt < total - 1) {
            List<int[]> nbrs = neighbors(r, c);
            for (int[] nb : nbrs)
                if (countNeighbors(nb[0], nb[1]) == 0)
                    return true;
        }
        return false;
    }

    private static void printResult() {
        for (int[] row : grid) {
            for (int i : row) {
                if (i == -1) continue;
                System.out.printf("%2d ", i);
            }
            System.out.println();
        }
    }
}
