#include <iostream>
#include <cstdlib>
#include <ctime>

int main()
{
    std::srand(std::time(0));
    int lower, upper, guess;
    std::cout << "Enter lower limit: ";
    std::cin >> lower;
    std::cout << "Enter upper limit: ";
    std::cin >> upper;
    int random_number = lower + std::rand() % ((upper + 1) - lower);

    do
    {
        std::cout << "Guess what number I have: ";
        std::cin >> guess;
        if (guess > random_number)
            std::cout << "Your guess is too high\n";
        else if (guess < random_number)
            std::cout << "Your guess is too low\n";
        else
            std::cout << "You got it!\n";
    } while (guess != random_number);

    return 0;
}
