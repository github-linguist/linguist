#include <fstream>
#include <iostream>
#include <set>
#include <string>

int main()
{
    std::ifstream input("unixdict.txt");
    if (!input) {
        return 1; // couldn't open input file
    }

    std::set<std::string> words; // previous words
    std::string word; // current word
    size_t count = 0; // pair count

    while (input >> word) {
        std::string drow(word.rbegin(), word.rend()); // reverse
        if (words.find(drow) == words.end()) { // pair not found
            words.insert(word);
        } else { // pair found
            if (count < 5) {
                std::cout << word << ' ' << drow << '\n';
            }
            ++count;
        }
    }
    std::cout << "\nSemordnilap pairs: " << count << '\n';
}
