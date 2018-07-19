#include <stdio.h>

/* yes, bubble sort */
void bubble_sort(int *idx, int n_idx, int *buf)
{
        int i, j, tmp;
#define for_ij for (i = 0; i < n_idx; i++) for (j = i + 1; j < n_idx; j++)
#define sort(a, b) if (a < b) { tmp = a; a = b; b = tmp;}
        for_ij { sort(idx[j], idx[i]);          }
        for_ij { sort(buf[idx[j]], buf[idx[i]]);}
#undef for_ij
#undef sort
}

int main()
{
        int values[] = {7, 6, 5, 4, 3, 2, 1, 0};
        int idx[] = {6, 1, 7};
        int i;

        printf("before sort:\n");
        for (i = 0; i < 8; i++)
                printf("%d ", values[i]);

        printf("\n\nafter sort:\n");
        bubble_sort(idx, 3, values);

        for (i = 0; i < 8; i++)
                printf("%d ", values[i]);
        printf("\n");

        return 0;
}
