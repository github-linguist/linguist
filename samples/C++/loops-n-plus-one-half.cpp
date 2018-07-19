#include <iostream>

int main()
{
  for (int i = 1; ; i++)
  {
    std::cout << i;
    if (i == 10)
      break;
    std::cout << ", ";
  }
  std::cout << std::endl;
  return 0;
}
