#include <stddef.h>

static void insertion_sort(int *a, const size_t n) {
    size_t i, j;
    int value;
    for (i = 1; i < n; i++) {
        value = a[i];
        for (j = i; j > 0 && value < a[j - 1]; j--) {
            a[j] = a[j - 1];
        }
        a[j] = value;
    }
}

int main(void) {
    int a[] = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1};
    insertion_sort(a, sizeof a / sizeof a[0]);
    return 0;
}
