void Combsort11(double a[], int nElements)
{
  int i, j, gap, swapped = 1;
  double temp;

  gap = nElements;
  while (gap > 1 || swapped == 1)
  {
    gap = gap * 10 / 13;
    if (gap == 9 || gap == 10) gap = 11;
    if (gap < 1) gap = 1;
    swapped = 0;
    for (i = 0, j = gap; j < nElements; i++, j++)
    {
      if (a[i] > a[j])
      {
        temp = a[i];
        a[i] = a[j];
        a[j] = temp;
        swapped = 1;
      }
    }
  }
}
