#include <algorithm>

template<typename Iter>
void insertion_sort(Iter beg, Iter end)
{
    for (Iter i = beg; i != end; ++i)
        std::rotate(std::upper_bound(beg, i, *i), i, i+1);
}
