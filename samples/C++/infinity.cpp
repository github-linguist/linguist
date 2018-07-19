#include <limits>

double inf()
{
  if (std::numeric_limits<double>::has_infinity)
    return std::numeric_limits<double>::infinity();
  else
    return std::numeric_limits<double>::max();
}
