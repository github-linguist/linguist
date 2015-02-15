#include <algorithm>
#include <string>
#include <vector>
#include <iostream>

template<class T>
void print(const std::vector<T> &vec)
{
    for (typename std::vector<T>::const_iterator i = vec.begin(); i != vec.end(); ++i)
    {
        std::cout << *i;
        if ((i + 1) != vec.end())
            std::cout << ",";
    }
    std::cout << std::endl;
}

int main()
{
    //Permutations for strings
    std::string example("Hello");
    std::sort(example.begin(), example.end());
    do {
        std::cout << example << '\n';
    } while (std::next_permutation(example.begin(), example.end()));

    // And for vectors
    std::vector<int> another;
    another.push_back(1234);
    another.push_back(4321);
    another.push_back(1234);
    another.push_back(9999);

    std::sort(another.begin(), another.end());
    do {
        print(another);
    } while (std::next_permutation(another.begin(), another.end()));

    return 0;
}
