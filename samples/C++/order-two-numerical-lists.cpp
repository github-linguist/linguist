#include <iostream>
#include <vector>

int main() {
  std::vector<int> a;
  a.push_back(1);
  a.push_back(2);
  a.push_back(1);
  a.push_back(3);
  a.push_back(2);
  std::vector<int> b;
  b.push_back(1);
  b.push_back(2);
  b.push_back(0);
  b.push_back(4);
  b.push_back(4);
  b.push_back(0);
  b.push_back(0);
  b.push_back(0);

  std::cout << std::boolalpha << (a < b) << std::endl; // prints "false"
  return 0;
}
