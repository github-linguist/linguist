public class Blah {

  public static void main(String[] args) {
    print2dArray(getSpiralArray(5));
  }

  public static int[][] getSpiralArray(int dimension) {
    int[][] spiralArray = new int[dimension][dimension];

    int numConcentricSquares = (int) Math.ceil((dimension) / 2.0);

    int j;
    int sideLen = dimension;
    int currNum = 0;

    for (int i = 0; i < numConcentricSquares; i++) {
      // do top side
      for (j = 0; j < sideLen; j++) {
        spiralArray[i][i + j] = currNum++;
      }

      // do right side
      for (j = 1; j < sideLen; j++) {
        spiralArray[i + j][dimension - 1 - i] = currNum++;
      }

      // do bottom side
      for (j = sideLen - 2; j > -1; j--) {
        spiralArray[dimension - 1 - i][i + j] = currNum++;
      }

      // do left side
      for (j = sideLen - 2; j > 0; j--) {
        spiralArray[i + j][i] = currNum++;
      }

      sideLen -= 2;
    }

    return spiralArray;
  }

  public static void print2dArray(int[][] array) {
    for (int[] row : array) {
      for (int elem : row) {
        System.out.printf("%3d", elem);
      }
      System.out.println();
    }
  }
}
