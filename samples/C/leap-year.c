#include <stdio.h>
#include <stdbool.h>

bool is_leap_year(int year)
{
    return !(year % 4) && year % 100 || !(year % 400);
}

int main()
{
    int test_case[] = {1900, 1994, 1996, 1997, 2000}, key, end;
    for (key = 0, end = sizeof(test_case)/sizeof(test_case[0]); key < end; ++key) {
        int year = test_case[key];
        printf("%d is %sa leap year.\n", year, is_leap_year(year) ? "" : "not ");
    }
}
