#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>

bool ordered(const std::string &word)
{
    return std::is_sorted(word.begin(), word.end()); // C++11
}

int main()
{
    std::ifstream infile("unixdict.txt");
    if (!infile) {
        std::cerr << "Can't open word file\n";
        return -1;
    }

    std::vector<std::string> words;
    std::string word;
    int longest = 0;

    while (std::getline(infile, word)) {
        int length = word.length();
        if (length < longest) continue; // don't test short words

        if (ordered(word)) {
            if (longest < length) {
                longest = length; // set new minimum length
                words.clear(); // reset the container
            }
            words.push_back(word);
        }
    }
    std::copy(words.begin(), words.end(), std::ostream_iterator<std::string>(std::cout, "\n"));
}
