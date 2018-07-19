#include <iomanip>
#include <iostream>
#include <tuple>

typedef std::tuple<double,double> coeff_t; // coefficients type
typedef coeff_t (*func_t)(int); // callback function type

double calc(func_t func, int n)
{
    double a, b, temp = 0;
    for (; n > 0; --n) {
        std::tie(a, b) = func(n);
        temp = b / (a + temp);
    }
    std::tie(a, b) = func(0);
    return a + temp;
}

coeff_t sqrt2(int n)
{
    return coeff_t(n > 0 ? 2 : 1, 1);
}

coeff_t napier(int n)
{
    return coeff_t(n > 0 ? n : 2, n > 1 ? n - 1 : 1);
}

coeff_t pi(int n)
{
    return coeff_t(n > 0 ? 6 : 3, (2 * n - 1) * (2 * n - 1));
}

int main()
{
    std::streamsize old_prec = std::cout.precision(15); // set output digits
    std::cout
        << calc(sqrt2, 20) << '\n'
        << calc(napier, 15) << '\n'
        << calc(pi, 10000) << '\n'
        << std::setprecision(old_prec); // reset precision
}
