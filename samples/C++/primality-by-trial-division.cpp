#include <cmath>

bool is_prime(unsigned int n)
{
    if (n <= 1)
        return false;
    if (n == 2)
        return true;
    for (unsigned int i = 2; i <= sqrt(n); ++i)
        if (n % i == 0)
            return false;
    return true;
}
