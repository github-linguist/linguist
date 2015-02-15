// Calculate the Digital Root and Additive Persistance of an Integer - Compiles with gcc4.7
//
// Nigel Galloway. July 23rd., 2012
//
#include <iostream>
#include <cmath>
#include <tuple>

std::tuple<const unsigned long long int,int,int> DigitalRoot(const unsigned long long int digits, const int BASE = 10) {
    int x = SumDigits(digits,BASE);
    int ap = 1;
    while (x >= BASE) {
        x = SumDigits(x,BASE);
        ap++;
    }
    return std::make_tuple(digits,ap,x);
}

int main() {
    const unsigned long long int ip[] = {961038,923594037444,670033,448944221089};
    for (auto i:ip){
        auto res = DigitalRoot(i);
        std::cout << std::get<0>(res) << " has digital root " << std::get<2>(res) << " and additive persistance " << std::get<1>(res) << "\n";
    }
    std::cout << "\n";
    const unsigned long long int hip[] = {0x7e0,0x14e344,0xd60141,0x12343210};
    for (auto i:hip){
        auto res = DigitalRoot(i,16);
        std::cout << std::hex << std::get<0>(res) << " has digital root " << std::get<2>(res) << " and additive persistance " << std::get<1>(res) << "\n";
    }
    return 0;
}
