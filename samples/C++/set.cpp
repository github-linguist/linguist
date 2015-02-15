#include <set>
#include <iostream>
#include <iterator>
#include <algorithm>

namespace set_display {
template <class T>
std::ostream& operator<<(std::ostream& os, const std::set<T>& set)
{
    os << '[';
    if (!set.empty()) {
        std::copy(set.begin(), --set.end(), std::ostream_iterator<T>(os, ", "));
        os << *--set.end();
    }
    return os << ']';
}
}

template <class T>
bool contains(const std::set<T>& set, const T& key)
{
    return set.count(key) != 0;
}

template <class T>
std::set<T> set_union(const std::set<T>& a, const std::set<T>& b)
{
    std::set<T> result;
    std::set_union(a.begin(), a.end(), b.begin(), b.end(), std::inserter(result, result.end()));
    return result;
}

template <class T>
std::set<T> set_intersection(const std::set<T>& a, const std::set<T>& b)
{
    std::set<T> result;
    std::set_intersection(a.begin(), a.end(), b.begin(), b.end(), std::inserter(result, result.end()));
    return result;
}

template <class T>
std::set<T> set_difference(const std::set<T>& a, const std::set<T>& b)
{
    std::set<T> result;
    std::set_difference(a.begin(), a.end(), b.begin(), b.end(), std::inserter(result, result.end()));
    return result;
}

template <class T>
bool is_subset(const std::set<T>& set, const std::set<T>& subset)
{
    return std::includes(set.begin(), set.end(), subset.begin(), subset.end());
}

int main()
{
    using namespace set_display;
    std::set<int> a{2, 5, 7, 5, 9, 2}; //C++11 initialization syntax
    std::set<int> b{1, 5, 9, 7, 4 };
    std::cout << "a = " << a << '\n';
    std::cout << "b = " << b << '\n';

    int value1 = 8, value2 = 5;
    std::cout << "Set a " << (contains(a, value1) ? "contains " : "does not contain ") << value1 << '\n';
    std::cout << "Set a " << (contains(a, value2) ? "contains " : "does not contain ") << value2 << '\n';

    std::cout << "Union of a and b: " << set_union(a, b) << '\n';
    std::cout << "Intersection of a and b: " << set_intersection(a, b) << '\n';
    std::cout << "Difference of a and b: " << set_difference(a, b) << '\n';

    std::set<int> sub{5, 9};
    std::cout << "Set b " << (is_subset(a, b) ? "is" : "is not") << " a subset of a\n";
    std::cout << "Set " << sub << ' ' << (is_subset(a, sub) ? "is" : "is not") << " a subset of a\n";

    std::set<int> copy = a;
    std::cout << "a " << (a == copy ? "equals " : "does not equal ") << copy << '\n';

    return 0;
}
