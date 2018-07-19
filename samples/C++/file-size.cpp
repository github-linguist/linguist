#include <iostream>
#include <fstream>

std::ios::off_type getFileSize(const char *filename) {
  std::ifstream f(filename);
  std::ios::pos_type begin = f.tellg();
  f.seekg(0, std::ios::end);
  std::ios::pos_type end = f.tellg();
  return end - begin;
}

int main() {
  std::cout << getFileSize("input.txt") << std::endl;
  std::cout << getFileSize("/input.txt") << std::endl;
  return 0;
}
