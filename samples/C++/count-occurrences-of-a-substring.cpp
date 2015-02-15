#include <iostream>
#include <string>

// returns count of non-overlapping occurrences of 'sub' in 'str'
int countSubstring(const std::string& str, const std::string& sub)
{
    if (sub.length() == 0) return 0;
    int count = 0;
    for (size_t offset = str.find(sub); offset != std::string::npos;
	 offset = str.find(sub, offset + sub.length()))
    {
        ++count;
    }
    return count;
}

int main()
{
    std::cout << countSubstring("the three truths", "th")    << '\n';
    std::cout << countSubstring("ababababab", "abab")        << '\n';
    std::cout << countSubstring("abaabba*bbaba*bbab", "a*b") << '\n';

    return 0;
}
