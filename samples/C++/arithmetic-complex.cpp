#include <iostream>
#include <complex>
using std::complex;

void complex_operations() {
  complex<double> a(1.0, 1.0);
  complex<double> b(3.14159, 1.25);

  // addition
  std::cout << a + b << std::endl;
  // multiplication
  std::cout << a * b << std::endl;
  // inversion
  std::cout << 1.0 / a << std::endl;
  // negation
  std::cout << -a << std::endl;
  // conjugate
  std::cout << std::conj(a) << std::endl;
}
