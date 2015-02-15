#include <string>
#include <cstdlib>
#include <iostream>
#include <cassert>
#include <algorithm>
#include <vector>

std::string allowed_chars = " ABCDEFGHIJKLMNOPQRSTUVWXYZ";

// class selection contains the fitness function, encapsulates the
// target string and allows access to it's length. The class is only
// there for access control, therefore everything is static. The
// string target isn't defined in the function because that way the
// length couldn't be accessed outside.
class selection
{
public:
  // this function returns 0 for the destination string, and a
  // negative fitness for a non-matching string. The fitness is
  // calculated as the negated sum of the circular distances of the
  // string letters with the destination letters.
  static int fitness(std::string candidate)
  {
    assert(target.length() == candidate.length());

    int fitness_so_far = 0;

    for (int i = 0; i < target.length(); ++i)
    {
      int target_pos = allowed_chars.find(target[i]);
      int candidate_pos = allowed_chars.find(candidate[i]);
      int diff = std::abs(target_pos - candidate_pos);
      fitness_so_far -= std::min(diff, int(allowed_chars.length()) - diff);
    }

    return fitness_so_far;
  }

  // get the target string length
  static int target_length() { return target.length(); }
private:
  static std::string target;
};

std::string selection::target = "METHINKS IT IS LIKE A WEASEL";

// helper function: cyclically move a character through allowed_chars
void move_char(char& c, int distance)
{
  while (distance < 0)
    distance += allowed_chars.length();
  int char_pos = allowed_chars.find(c);
  c = allowed_chars[(char_pos + distance) % allowed_chars.length()];
}

// mutate the string by moving the characters by a small random
// distance with the given probability
std::string mutate(std::string parent, double mutation_rate)
{
  for (int i = 0; i < parent.length(); ++i)
    if (std::rand()/(RAND_MAX + 1.0) < mutation_rate)
    {
      int distance = std::rand() % 3 + 1;
      if(std::rand()%2 == 0)
        move_char(parent[i], distance);
      else
        move_char(parent[i], -distance);
    }
  return parent;
}

// helper function: tell if the first argument is less fit than the
// second
bool less_fit(std::string const& s1, std::string const& s2)
{
  return selection::fitness(s1) < selection::fitness(s2);
}

int main()
{
  int const C = 100;

  std::srand(time(0));

  std::string parent;
  for (int i = 0; i < selection::target_length(); ++i)
  {
    parent += allowed_chars[std::rand() % allowed_chars.length()];
  }

  int const initial_fitness = selection::fitness(parent);

  for(int fitness = initial_fitness;
      fitness < 0;
      fitness = selection::fitness(parent))
  {
    std::cout << parent << ": " << fitness << "\n";
    double const mutation_rate = 0.02 + (0.9*fitness)/initial_fitness;
    typedef std::vector<std::string> childvec;
    childvec childs;
    childs.reserve(C+1);

    childs.push_back(parent);
    for (int i = 0; i < C; ++i)
      childs.push_back(mutate(parent, mutation_rate));

    parent = *std::max_element(childs.begin(), childs.end(), less_fit);
  }
  std::cout << "final string: " << parent << "\n";
}
