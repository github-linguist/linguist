#include <string>
#include <sstream>

std::string lookandsay(const std::string &s)
{
  std::ostringstream r;

  for (unsigned int i = 0; i != s.length(); ) {
    unsigned int new_i = s.find_first_not_of(s[i], i+1);
    if (new_i == std::string::npos)
      new_i = s.length();

    r << new_i - i << s[i];
    i = new_i;
  }
  return r.str();
}

#include <iostream>

int main()
{
  std::string laf = "1";

  std::cout << laf << std::endl;
  for (int i = 0; i < 10; i++) {
    laf = lookandsay(laf);
    std::cout << laf << std::endl;
  }

  return 0;
}
