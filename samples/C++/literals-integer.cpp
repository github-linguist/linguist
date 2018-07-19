#include <iostream>

int main()
{
  std::cout << ( (727 == 0x2d7) &&
                 (727 == 01327)     ? "true" : "false")
            << std::endl;

  return 0;
}
