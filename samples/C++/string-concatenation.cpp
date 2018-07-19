#include <string>
#include <iostream>

int main() {
   std::string s = "hello";
   std::cout << s << " literal" << std::endl;
   std::string s2 = s + " literal";
   std::cout << s2 << std::endl;
   return 0;
}
