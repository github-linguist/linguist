#include <algorithm>
#include <iterator>

template<typename RandomAccessIterator>
void gnomeSort(RandomAccessIterator begin, RandomAccessIterator end) {
  RandomAccessIterator i = begin + 1;
  RandomAccessIterator j = begin + 2;

  while(i < end) {
    if(*(i - 1) <= *i) {
      i = j;
      ++j;
    } else {
      std::iter_swap(i - 1, i);
      --i;
      if(i == begin) {
        i = j;
        ++j;
      }
    }
  }
}
