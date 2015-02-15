#include <stdexcept>

class tiny_int
{
public:
  tiny_int(int i):
    value(i)
  {
    if (value < 1)
      throw std::out_of_range("tiny_int: value smaller than 1");
    if (value > 10)
      throw std::out_of_range("tiny_int: value larger than 10");
  }
  operator int() const
  {
    return value;
  }
  tiny_int& operator+=(int i)
  {
    // by assigning to *this instead of directly modifying value, the
    // constructor is called and thus the check is enforced
    *this = value + i;
    return *this;
  }
  tiny_int& operator-=(int i)
  {
    *this = value - i;
    return *this;
  }
  tiny_int& operator*=(int i)
  {
    *this = value * i;
    return *this;
  }
  tiny_int& operator/=(int i)
  {
    *this = value / i;
    return *this;
  }
  tiny_int& operator<<=(int i)
  {
    *this = value << i;
    return *this;
  }
  tiny_int& operator>>=(int i)
  {
    *this = value >> i;
    return *this;
  }
  tiny_int& operator&=(int i)
  {
    *this = value & i;
    return *this;
  }
  tiny_int& operator|=(int i)
  {
    *this = value | i;
    return *this;
  }
private:
  unsigned char value; // we don't need more space
};
