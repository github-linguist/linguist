#include <algorithm>
#include <iostream>
#include <string>

std::string stripchars(std::string str, const std::string &chars)
{
    str.erase(
        std::remove_if(str.begin(), str.end(), [&](char c){
            return chars.find(c) != std::string::npos;
        }),
        str.end()
    );
    return str;
}

int main()
{
    std::cout << stripchars("She was a soul stripper. She took my heart!", "aei") << '\n';
    return 0;
}
