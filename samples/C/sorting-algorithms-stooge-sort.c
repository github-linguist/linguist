#include <stdio.h>

#define SWAP(r,s)  do{ t=r; r=s; s=t; } while(0)

void StoogeSort(int a[], int i, int j)
{
   int t;

   if (a[j] < a[i]) SWAP(a[i], a[j]);
   if (j - i > 1)
   {
       t = (j - i + 1) / 3;
       StoogeSort(a, i, j - t);
       StoogeSort(a, i + t, j);
       StoogeSort(a, i, j - t);
   }
}

int main(int argc, char *argv[])
{
   int nums[] = {1, 4, 5, 3, -6, 3, 7, 10, -2, -5, 7, 5, 9, -3, 7};
   int i, n;

   n = sizeof(nums)/sizeof(int);
   StoogeSort(nums, 0, n-1);

   for(i = 0; i <= n-1; i++)
      printf("%5d", nums[i]);

   return 0;
}
