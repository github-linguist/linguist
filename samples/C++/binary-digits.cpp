#include <iostream>
#include <bitset>
#include <string>
#include <climits>

void print_bin(int n)
{
    // convert to binary, then to string
    std::string str = std::bitset<CHAR_BIT * sizeof n>(n).to_string();
    // trim leading zeroes
    if(n == 0)
       str = "0";
    else
       str = str.substr(str.find('1'));
    // output
    std::cout << str << '\n';
}

int main()
{
    print_bin(0);
    print_bin(5);
    print_bin(50);
    print_bin(9000);
}
