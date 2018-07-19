#include <iostream>
#include <cstdlib>
#include <ctime>

int main()
{
    srand(time(0));
    int n = 1 + (rand() % 10);
    int g;
    std::cout << "I'm thinking of a number between 1 and 10.\nTry to guess it! ";
    while(true)
    {
        std::cin >> g;
        if (g == n)
            break;
        else
            std::cout << "That's not my number.\nTry another guess! ";
    }
    std::cout << "You've guessed my number!";
    return 0;
}
