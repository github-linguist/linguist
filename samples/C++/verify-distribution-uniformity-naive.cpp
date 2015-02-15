#include <map>
#include <iostream>
#include <cmath>

template<typename F>
 bool test_distribution(F f, int calls, double delta)
{
  typedef std::map<int, int> distmap;
  distmap dist;

  for (int i = 0; i < calls; ++i)
    ++dist[f()];

  double mean = 1.0/dist.size();

  bool good = true;

  for (distmap::iterator i = dist.begin(); i != dist.end(); ++i)
  {
    if (std::abs((1.0 * i->second)/calls - mean) > delta)
    {
      std::cout << "Relative frequency " << i->second/(1.0*calls)
                << " of result " << i->first
                << " deviates by more than " << delta
                << " from the expected value " << mean << "\n";
      good = false;
    }
  }

  return good;
}
