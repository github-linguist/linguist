#include <iostream>

template<typename T>
 void print(T const& t)
{
  std::cout << t;
}

template<typename First, typename ... Rest>
 void print(First const& first, Rest const& ... rest)
{
  std::cout << first;
  print(rest ...);
}

int main()
{
  int i = 10;
  std::string s = "Hello world";
  print("i = ", i, " and s = \"", s, "\"\n");
}
