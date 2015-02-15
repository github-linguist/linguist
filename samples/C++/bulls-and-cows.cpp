#include <iostream>
#include <string>
#include <algorithm>
#include <cstdlib>

bool contains_duplicates(std::string s)
{
  std::sort(s.begin(), s.end());
  return std::adjacent_find(s.begin(), s.end()) != s.end();
}

void game()
{
  typedef std::string::size_type index;

  std::string symbols = "0123456789";
  unsigned int const selection_length = 4;

  std::random_shuffle(symbols.begin(), symbols.end());
  std::string selection = symbols.substr(0, selection_length);
  std::string guess;
  while (std::cout << "Your guess? ", std::getline(std::cin, guess))
  {
    if (guess.length() != selection_length
        || guess.find_first_not_of(symbols) != std::string::npos
        || contains_duplicates(guess))
    {
      std::cout << guess << " is not a valid guess!";
      continue;
    }

    unsigned int bulls = 0;
    unsigned int cows = 0;
    for (index i = 0; i != selection_length; ++i)
    {
      index pos = selection.find(guess[i]);
      if (pos == i)
        ++bulls;
      else if (pos != std::string::npos)
        ++cows;
    }
    std::cout << bulls << " bulls, " << cows << " cows.\n";
    if (bulls == selection_length)
    {
      std::cout << "Congratulations! You have won!\n";
      return;
    }
  }
  std::cerr << "Oops! Something went wrong with input, or you've entered end-of-file!\nExiting ...\n";
  std::exit(EXIT_FAILURE);
}

int main()
{
  std::cout << "Welcome to bulls and cows!\nDo you want to play? ";
  std::string answer;
  while (true)
  {
    while (true)
    {
      if (!std::getline(std::cin, answer))
      {
        std::cout << "I can't get an answer. Exiting.\n";
        return EXIT_FAILURE;
      }
      if (answer == "yes" || answer == "Yes" || answer == "y" || answer == "Y")
        break;
      if (answer == "no" || answer == "No" || answer == "n" || answer == "N")
      {
        std::cout << "Ok. Goodbye.\n";
        return EXIT_SUCCESS;
      }
      std::cout << "Please answer yes or no: ";
    }
    game();
    std::cout << "Another game? ";
  }
}
