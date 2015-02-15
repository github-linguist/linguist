#include <iostream>
#include <iterator>
#include <cstddef>

template<typename InIter>
 void extract_ranges(InIter begin, InIter end, std::ostream& os)
{
  if (begin == end)
    return;

  int current = *begin++;
  os << current;
  int count = 1;

  while (begin != end)
  {
    int next = *begin++;
    if (next == current+1)
      ++count;
    else
    {
      if (count > 2)
        os << '-';
      else
        os << ',';
      if (count > 1)
        os << current << ',';
      os << next;
      count = 1;
    }
    current = next;
  }

  if (count > 1)
    os << (count > 2? '-' : ',') << current;
}

template<typename T, std::size_t n>
 T* end(T (&array)[n])
{
  return array+n;
}

int main()
{
  int data[] = { 0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
                 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
                 25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
                 37, 38, 39 };

  extract_ranges(data, end(data), std::cout);
  std::cout << std::endl;
}
