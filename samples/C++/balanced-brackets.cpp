#include <algorithm>
#include <iostream>
#include <string>

std::string generate(int n, char left = '[', char right = ']')
{
    std::string str(std::string(n, left) + std::string(n, right));
    std::random_shuffle(str.begin(), str.end());
    return str;
}

bool balanced(const std::string &str, char left = '[', char right = ']')
{
    int count = 0;
    for (std::string::const_iterator it = str.begin(); it != str.end(); ++it)
    {
        if (*it == left)
            count++;
        else if (*it == right)
            if (--count < 0) return false;
    }
    return count == 0;
}

int main()
{
    srand(time(NULL)); // seed rng
    for (int i = 0; i < 9; ++i)
    {
        std::string s(generate(i));
        std::cout << (balanced(s) ? " ok: " : "bad: ") << s << "\n";
    }
}
