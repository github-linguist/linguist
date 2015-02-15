#include <iostream>
#include <string>
#include <algorithm>

int main()
{
  std::string s;
  std::getline(std::cin, s);
  std::reverse(s.begin(), s.end()); // modifies s
  std::cout << s << std::endl;
  return 0;
}
