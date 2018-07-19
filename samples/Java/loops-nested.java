import java.util.Random;

public class NestedLoopTest {
    public static final Random gen = new Random();
    public static void main(String[] args) {
        int[][] a = new int[10][10];
        for (int i = 0; i < a.length; i++)
            for (int j = 0; j < a[i].length; j++)
                a[i][j] = gen.nextInt(20) + 1;

        Outer:for (int i = 0; i < a.length; i++) {
            for (int j = 0; j < a[i].length; j++) {
                System.out.print(" " + a[i][j]);
                if (a[i][j] == 20)
                    break Outer; //adding a label breaks out of all loops up to and including the labelled loop
            }
            System.out.println();
        }
        System.out.println();
    }
}
