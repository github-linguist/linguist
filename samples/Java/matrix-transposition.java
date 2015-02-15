import java.util.Arrays;
public class Transpose{
       public static void main(String[] args){
               double[][] m = {{1, 1, 1, 1},
                               {2, 4, 8, 16},
                               {3, 9, 27, 81},
                               {4, 16, 64, 256},
                               {5, 25, 125, 625}};
               double[][] ans = new double[m[0].length][m.length];
               for(int rows = 0; rows < m.length; rows++){
                       for(int cols = 0; cols < m[0].length; cols++){
                               ans[cols][rows] = m[rows][cols];
                       }
               }
               for(double[] i:ans){//2D arrays are arrays of arrays
                       System.out.println(Arrays.toString(i));
               }
       }
}
