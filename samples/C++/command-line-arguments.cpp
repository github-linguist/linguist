#include <iostream>

int main(int argc, char* argv[])
{
  std::cout << "This program is named " << argv[0] << std::endl;
  std::cout << "There are " << argc-1 << " arguments given." << std::endl;
  for (int i = 1; i < argc; ++i)
    std::cout << "the argument #" << i << " is " << argv[i] << std::endl;

  return 0;
}
