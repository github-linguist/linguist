#include <iostream>

int main()
{
  int a, b;
  std::cin >> a >> b;
  std::cout << "a+b = " << a+b << "\n";
  std::cout << "a-b = " << a-b << "\n";
  std::cout << "a*b = " << a*b << "\n";
  std::cout << "a/b = " << a/b << ", remainder " << a%b << "\n";
  return 0;
}
