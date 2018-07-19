#include <algorithm>
#include <iostream>
#include <numeric>
#include <vector>

template <typename T>
std::vector<size_t> equilibrium(T first, T last)
{
    typedef typename std::iterator_traits<T>::value_type value_t;

    value_t left  = 0;
    value_t right = std::accumulate(first, last, value_t(0));
    std::vector<size_t> result;

    for (size_t index = 0; first != last; ++first, ++index)
    {
        right -= *first;
        if (left == right)
        {
            result.push_back(index);
        }
        left += *first;
    }
    return result;
}

template <typename T>
void print(const T& value)
{
    std::cout << value << "\n";
}

int main()
{
    const int data[] = { -7, 1, 5, 2, -4, 3, 0 };

    std::vector<size_t> indices(equilibrium(data, data + 7));

    std::for_each(indices.begin(), indices.end(), print<size_t>);
}
