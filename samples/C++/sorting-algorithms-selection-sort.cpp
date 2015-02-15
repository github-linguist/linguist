#include <algorithm>
#include <iterator>

template<typename ForwardIterator>
void selectionSort(ForwardIterator begin, ForwardIterator end) {
    for(ForwardIterator i = begin; i != end; ++i)
        std::iter_swap(i, std::min_element(i, end));
}
