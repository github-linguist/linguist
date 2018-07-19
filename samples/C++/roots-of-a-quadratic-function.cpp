#include <iostream>
#include <utility>
#include <complex>

typedef std::complex<double> complex;

std::pair<complex, complex>
 solve_quadratic_equation(double a, double b, double c)
{
  b /= a;
  c /= a;
  double discriminant = b*b-4*c;
  if (discriminant < 0)
    return std::make_pair(complex(-b/2, std::sqrt(-discriminant)/2),
                          complex(-b/2, -std::sqrt(-discriminant)/2));

  double root = std::sqrt(discriminant);
  double solution1 = (b > 0)? (-b - root)/2
                            : (-b + root)/2;

  return std::make_pair(solution1, c/solution1);
}

int main()
{
  std::pair<complex, complex> result = solve_quadratic_equation(1, -1e20, 1);
  std::cout << result.first << ", " << result.second << std::endl;
}
