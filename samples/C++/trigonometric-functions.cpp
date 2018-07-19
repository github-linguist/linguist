#include <iostream>
#include <cmath>

#ifdef M_PI // defined by all POSIX systems and some non-POSIX ones
double const pi = M_PI;
#else
double const pi = 4*std::atan(1);
#endif

double const degree = pi/180;

int main()
{
  std::cout << "=== radians ===\n";
  std::cout << "sin(pi/3) = " << std::sin(pi/3) << "\n";
  std::cout << "cos(pi/3) = " << std::cos(pi/3) << "\n";
  std::cout << "tan(pi/3) = " << std::tan(pi/3) << "\n";
  std::cout << "arcsin(1/2) = " << std::asin(0.5) << "\n";
  std::cout << "arccos(1/2) = " << std::acos(0.5) << "\n";
  std::cout << "arctan(1/2) = " << std::atan(0.5) << "\n";

  std::cout << "\n=== degrees ===\n";
  std::cout << "sin(60°) = " << std::sin(60*degree) << "\n";
  std::cout << "cos(60°) = " << std::cos(60*degree) << "\n";
  std::cout << "tan(60°) = " << std::tan(60*degree) << "\n";
  std::cout << "arcsin(1/2) = " << std::asin(0.5)/degree << "°\n";
  std::cout << "arccos(1/2) = " << std::acos(0.5)/degree << "°\n";
  std::cout << "arctan(1/2) = " << std::atan(0.5)/degree << "°\n";

  return 0;
}
