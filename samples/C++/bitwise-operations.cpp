#include <iostream>

void bitwise(int a, int b)
{
  std::cout << "a and b: " << (a & b)  << '\n'; // Note: parentheses are needed because & has lower precedence than <<
  std::cout << "a or b:  " << (a | b)  << '\n';
  std::cout << "a xor b: " << (a ^ b)  << '\n';
  std::cout << "not a:   " << ~a       << '\n';
  std::cout << "a shl b: " << (a << b) << '\n'; // Note: "<<" is used both for output and for left shift
  std::cout << "a shr b: " << (a >> b) << '\n'; // typically arithmetic right shift, but not guaranteed
  unsigned int c = a;
  std::cout << "c sra b: " << (c >> b) << '\n'; // logical right shift (guaranteed)
  // there are no rotation operators in C++
}
