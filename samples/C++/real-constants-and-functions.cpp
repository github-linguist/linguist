#include <iostream>
#include <cmath>

#ifdef M_E
static double euler_e = M_E;
#else
static double euler_e = std::exp(1); // standard fallback
#endif

#ifdef M_PI
static double pi = M_PI;
#else
static double pi = std::acos(-1);
#endif

int main()
{
  std::cout << "e = " << euler_e
            << "\npi = " << pi
            << "\nsqrt(2) = " << std::sqrt(2.0)
            << "\nln(e) = " << std::log(e)
            << "\nlg(100) = " << std::log10(100.0)
            << "\nexp(3) = " << std::exp(3.0)
            << "\n|-4.5| = " << std::abs(-4.5)   // or std::fabs(4.0); both work in C++
            << "\nfloor(4.5) = " << std::floor(4.5)
            << "\nceiling(4.5) = " << std::ceil(4.5)
            << "\npi^2 = " << std::pow(pi,2.0) << std::endl;
}
