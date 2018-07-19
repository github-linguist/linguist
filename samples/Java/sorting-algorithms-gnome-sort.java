public static void gnomeSort(int[] a)
{
  int i=1;
  int j=2;

  while(i < a.length) {
    if ( a[i-1] <= a[i] ) {
      i = j; j++;
    } else {
      int tmp = a[i-1];
      a[i-1] = a[i];
      a[i--] = tmp;
      i = (i==0) ? j++ : i;
    }
  }
}
