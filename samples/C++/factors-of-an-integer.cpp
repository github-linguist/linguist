#include <iostream>
#include <vector>
#include <algorithm>
#include <iterator>

std::vector<int> GenerateFactors(int n)
{
    std::vector<int> factors;
    factors.push_back(1);
    factors.push_back(n);
    for(int i = 2; i * i <= n; ++i)
    {
        if(n % i == 0)
        {
            factors.push_back(i);
            if(i * i != n)
                factors.push_back(n / i);
        }
    }

    std::sort(factors.begin(), factors.end());
    return factors;
}

int main()
{
    const int SampleNumbers[] = {3135, 45, 60, 81};

    for(size_t i = 0; i < sizeof(SampleNumbers) / sizeof(int); ++i)
    {
        std::vector<int> factors = GenerateFactors(SampleNumbers[i]);
        std::cout << "Factors of " << SampleNumbers[i] << " are:\n";
        std::copy(factors.begin(), factors.end(), std::ostream_iterator<int>(std::cout, "\n"));
        std::cout << std::endl;
    }
}
