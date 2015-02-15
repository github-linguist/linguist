#include <algorithm>
#include <fstream>
#include <functional>
#include <iostream>
#include <map>
#include <numeric>
#include <set>
#include <string>

bool is_deranged(const std::string& left, const std::string& right)
{
    return (left.size() == right.size()) &&
        (std::inner_product(left.begin(), left.end(), right.begin(), 0, std::plus<int>(), std::equal_to<char>()) == 0);
}

int main()
{
    std::ifstream input("unixdict.txt");
    if (!input) {
        std::cerr << "can't open input file\n";
        return EXIT_FAILURE;
    }

    typedef std::set<std::string> WordList;
    typedef std::map<std::string, WordList> AnagraMap;
    AnagraMap anagrams;

    std::pair<std::string, std::string> result;
    size_t longest = 0;

    for (std::string value; input >> value; /**/) {
        std::string key(value);
        std::sort(key.begin(), key.end());

        if (longest < value.length()) { // is it a long candidate?
            if (0 < anagrams.count(key)) { // is it an anagram?
                for (const auto& prior : anagrams[key]) {
                    if (is_deranged(prior, value)) { // are they deranged?
                        result = std::make_pair(prior, value);
                        longest = value.length();
                    }
                }
            }
        }
        anagrams[key].insert(value);
    }

    std::cout << result.first << ' ' << result.second << '\n';
    return EXIT_SUCCESS;
}
