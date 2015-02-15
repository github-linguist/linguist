#include <stdio.h>

int SumDigits(unsigned long long n, const int base) {
    int sum = 0;
    for (; n; n /= base)
    	sum += n % base;
    return sum;
}

int main() {
    printf("%d %d %d %d %d\n",
        SumDigits(1, 10),
        SumDigits(12345, 10),
        SumDigits(123045, 10),
        SumDigits(0xfe, 16),
        SumDigits(0xf0e, 16) );
    return 0;
}
