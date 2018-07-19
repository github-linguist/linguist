void gnome_sort(int *a, int n)
{
  int i=1, j=2, t;
# define swap(i, j) { t = a[i]; a[i] = a[j]; a[j] = t; }
  while(i < n) {
    if (a[i - 1] > a[i]) {
      swap(i - 1, i);
      if (--i) continue;
    }
    i = j++;
  }
# undef swap
}
