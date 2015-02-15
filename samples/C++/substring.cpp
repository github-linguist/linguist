#include <iostream>
#include <string>

int main()
{
  std::string s = "0123456789";

  int const n = 3;
  int const m = 4;
  char const c = '2';
  std::string const sub = "456";

  std::cout << s.substr(n, m)<< "\n";
  std::cout << s.substr(n) << "\n";
  std::cout << s.substr(0, s.size()-1) << "\n";
  std::cout << s.substr(s.find(c), m) << "\n";
  std::cout << s.substr(s.find(sub), m) << "\n";
}
