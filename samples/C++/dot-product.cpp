#include <iostream>
#include <numeric>

int main()
{
    int a[] = { 1, 3, -5 };
    int b[] = { 4, -2, -1 };

    std::cout << std::inner_product(a, a + sizeof(a) / sizeof(a[0]), b, 0) << std::endl;

    return 0;
}
