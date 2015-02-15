#include "boost/filesystem.hpp"
#include <string>
#include <iostream>

void testfile(std::string name)
{
  boost::filesystem::path file(name);
  if (exists(file))
  {
    if (is_directory(file))
      std::cout << name << " is a directory.\n";
    else
      std::cout << name << " is a non-directory file.\n";
  }
  else
    std::cout << name << " does not exist.\n";
}

int main()
{
  testfile("input.txt");
  testfile("docs");
  testfile("/input.txt");
  testfile("/docs");
}
