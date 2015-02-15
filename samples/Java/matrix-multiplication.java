public static double[][] mult(double a[][], double b[][]){//a[m][n], b[n][p]
   if(a.length == 0) return new double[0][0];
   if(a[0].length != b.length) return null; //invalid dims

   int n = a[0].length;
   int m = a.length;
   int p = b[0].length;

   double ans[][] = new double[m][p];

   for(int i = 0;i < m;i++){
      for(int j = 0;j < p;j++){
         for(int k = 0;k < n;k++){
            ans[i][j] += a[i][k] * b[k][j];
         }
      }
   }
   return ans;
}
