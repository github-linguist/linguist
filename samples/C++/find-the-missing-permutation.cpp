#include <algorithm>
#include <vector>
#include <set>
#include <iterator>
#include <iostream>
#include <string>

static const std::string GivenPermutations[] = {
  "ABCD","CABD","ACDB","DACB",
  "BCDA","ACBD","ADCB","CDAB",
  "DABC","BCAD","CADB","CDBA",
  "CBAD","ABDC","ADBC","BDCA",
  "DCBA","BACD","BADC","BDAC",
  "CBDA","DBCA","DCAB"
};
static const size_t NumGivenPermutations = sizeof(GivenPermutations) / sizeof(*GivenPermutations);

int main()
{
    std::vector<std::string> permutations;
    std::string initial = "ABCD";
    permutations.push_back(initial);

    while(true)
    {
        std::string p = permutations.back();
        std::next_permutation(p.begin(), p.end());
        if(p == permutations.front())
            break;
        permutations.push_back(p);
    }

    std::vector<std::string> missing;
    std::set<std::string> given_permutations(GivenPermutations, GivenPermutations + NumGivenPermutations);
    std::set_difference(permutations.begin(), permutations.end(), given_permutations.begin(),
        given_permutations.end(), std::back_inserter(missing));
    std::copy(missing.begin(), missing.end(), std::ostream_iterator<std::string>(std::cout, "\n"));
    return 0;
}
