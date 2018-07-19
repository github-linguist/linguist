#include <iostream>
#include <iomanip>

int main()
{
  std::cout << std::setfill('0') << std::setw(9) << std::fixed << std::setprecision(3) << 7.125 << std::endl;
  return 0;
}
