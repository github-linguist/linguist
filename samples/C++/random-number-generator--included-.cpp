#include <iostream>
#include <string>
#include <random>

int main()
{
    std::random_device rd;
    std::uniform_int_distribution<int> dist(1, 10);
    std::mt19937 mt(rd());

    std::cout << "Random Number (hardware): " << dist(rd) << std::endl;
    std::cout << "Mersenne twister (hardware seeded): " << dist(mt) << std::endl;
}
