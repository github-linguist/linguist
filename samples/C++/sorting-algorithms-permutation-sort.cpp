#include <algorithm>

template<typename ForwardIterator>
 void permutation_sort(ForwardIterator begin, ForwardIterator end)
{
  while (std::next_permutation(begin, end))
  {
    // -- this block intentionally left empty --
  }
}
