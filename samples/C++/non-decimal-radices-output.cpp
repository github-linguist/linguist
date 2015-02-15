#include <iostream>
#include <iomanip>

int main()
{
  for (int i = 0; i <= 33; i++)
    std::cout << std::setw(6) << std::dec << i << " "
              << std::setw(6) << std::hex << i << " "
              << std::setw(6) << std::oct << i << std::endl;

  return 0;
}
