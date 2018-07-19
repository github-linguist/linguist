#include <algorithm>
#include <cassert>

template<typename Ty> Ty max(unsigned int count, Ty values[]) {
     assert(count > 0);
     return *std::max_element(values, values + count);
}
