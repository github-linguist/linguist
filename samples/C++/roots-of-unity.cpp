#include <complex>
#include <cmath>
#include <iostream>

double const pi = 4 * std::atan(1);

int main()
{
  for (int n = 2; n <= 10; ++n)
  {
    std::cout << n << ": ";
    for (int k = 0; k < n; ++k)
      std::cout << std::polar(1, 2*pi*k/n) << " ";
    std::cout << std::endl;
  }
}
