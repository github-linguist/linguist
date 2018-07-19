#include <string>
#include <cstdlib>
#include <algorithm>
#include <cassert>

std::string const digits = "0123456789abcdefghijklmnopqrstuvwxyz";

std::string to_base(unsigned long num, int base)
{
  if (num == 0)
    return "0";

  std::string result;
  while (num > 0) {
    std::ldiv_t temp = std::div(num, (long)base);
    result += digits[temp.rem];
    num = temp.quot;
  }
  std::reverse(result.begin(), result.end());
  return result;
}

unsigned long from_base(std::string const& num_str, int base)
{
  unsigned long result = 0;
  for (std::string::size_type pos = 0; pos < num_str.length(); ++pos)
    result = result * base + digits.find(num_str[pos]);
  return result;
}
