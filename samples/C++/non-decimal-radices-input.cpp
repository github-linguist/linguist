#include <iostream>
#include <sstream>

int main()
{
  int num;

  std::istringstream("0123459") >> num;
  std::cout << num << std::endl; // prints 123459

  std::istringstream("0123459") >> std::dec >> num;
  std::cout << num << std::endl; // prints 123459

  std::istringstream("abcf123") >> std::hex >> num;
  std::cout << num << std::endl; // prints 180154659

  std::istringstream("7651") >> std::oct >> num;
  std::cout << num << std::endl; // prints 4009

  // binary not supported

  return 0;
}
