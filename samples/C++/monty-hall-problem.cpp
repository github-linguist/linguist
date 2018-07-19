#include <iostream>
#include <cstdlib>
#include <ctime>

int randint(int n)
{
  return (1.0*n*std::rand())/(1.0+RAND_MAX);
}

int other(int doorA, int doorB)
{
  int doorC;
  if (doorA == doorB)
  {
    doorC = randint(2);
    if (doorC >= doorA)
      ++doorC;
  }
  else
  {
    for (doorC = 0; doorC == doorA || doorC == doorB; ++doorC)
    {
      // empty
    }
  }
  return doorC;
}

int check(int games, bool change)
{
  int win_count = 0;
  for (int game = 0; game < games; ++game)
  {
    int const winning_door = randint(3);
    int const original_choice = randint(3);
    int open_door = other(original_choice, winning_door);

    int const selected_door = change?
                                other(open_door, original_choice)
                              : original_choice;

    if (selected_door == winning_door)
      ++win_count;
  }

  return win_count;
}

int main()
{
  std::srand(std::time(0));

  int games = 10000;
  int wins_stay = check(games, false);
  int wins_change = check(games, true);
  std::cout << "staying: " << 100.0*wins_stay/games << "%, changing: " << 100.0*wins_change/games << "%\n";
}
