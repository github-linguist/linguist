#include <iostream>

int main()
{
  unsigned i = 0;
  do
  {
    std::cout << std::oct << i << std::endl;
    ++i;
  } while(i != 0);
  return 0;
}
