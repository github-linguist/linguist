#include <iostream>
#include <utility>

template<typename tVal>
tVal map_value(std::pair<tVal,tVal> a, std::pair<tVal, tVal> b, tVal inVal)
{
  tVal inValNorm = inVal - a.first;
  tVal aUpperNorm = a.second - a.first;
  tVal normPosition = inValNorm / aUpperNorm;

  tVal bUpperNorm = b.second - b.first;
  tVal bValNorm = normPosition * bUpperNorm;
  tVal outVal = b.first + bValNorm;

  return outVal;
}

int main()
{
  std::pair<float,float> a(0,10), b(-1,0);

  for(float value = 0.0; 10.0 >= value; ++value)
    std::cout << "map_value(" << value << ") = " << map_value(a, b, value) << std::endl;

  return 0;
}
