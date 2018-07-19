#include <iostream>
#include <iterator>
#include <vector>
#include <ppl.h> // MSVC++
#include <concurrent_vector.h> // MSVC++

struct Factors
{
    int number;
    std::vector<int> primes;
};

const int data[] =
{
    12757923, 12878611, 12878893, 12757923, 15808973, 15780709, 197622519
};

int main()
{
    // concurrency-safe container replaces std::vector<>
    Concurrency::concurrent_vector<Factors> results;

    // parallel algorithm replaces std::for_each()
    Concurrency::parallel_for_each(std::begin(data), std::end(data), [&](int n)
    {
        Factors factors;
        factors.number = n;
        for (int f = 2; n > 1; ++f)
        {
            while (n % f == 0)
            {
                factors.primes.push_back(f);
                n /= f;
            }
        }
        results.push_back(factors); // add factorization to results
    });
    // end of parallel calculations

    // find largest minimal prime factor in results
    auto max = std::max_element(results.begin(), results.end(), [](const Factors &a, const Factors &b)
    {
        return a.primes.front() < b.primes.front();
    });

    // print number(s) and factorization
    std::for_each(results.begin(), results.end(), [&](const Factors &f)
    {
        if (f.primes.front() == max->primes.front())
        {
            std::cout << f.number << " = [ ";
            std::copy(f.primes.begin(), f.primes.end(), std::ostream_iterator<int>(std::cout, " "));
            std::cout << "]\n";
        }
    });
    return 0;
}
