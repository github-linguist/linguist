#include <iostream>
#include <sstream>
#include <iterator>
#include <climits>
#include <deque>

// parse a list of numbers with ranges
//
// arguments:
//  is:  the stream to parse
//  out: the output iterator the parsed list is written to.
//
// returns true if the parse was successful. false otherwise
template<typename OutIter>
 bool parse_number_list_with_ranges(std::istream& is, OutIter out)
{
  int number;
  // the list always has to start with a number
  while (is >> number)
  {
    *out++ = number;

    char c;
    if (is >> c)
      switch(c)
      {
      case ',':
        continue;
      case '-':
        {
          int number2;
          if (is >> number2)
          {
            if (number2 < number)
              return false;
            while (number < number2)
              *out++ = ++number;
            char c2;
            if (is >> c2)
              if (c2 == ',')
                continue;
              else
                return false;
            else
              return is.eof();
          }
          else
            return false;
        }
      default:
        return is.eof();
      }
    else
      return is.eof();
  }
  // if we get here, something went wrong (otherwise we would have
  // returned from inside the loop)
  return false;
}

int main()
{
  std::istringstream example("-6,-3--1,3-5,7-11,14,15,17-20");
  std::deque<int> v;
  bool success = parse_number_list_with_ranges(example, std::back_inserter(v));
  if (success)
  {
    std::copy(v.begin(), v.end()-1,
              std::ostream_iterator<int>(std::cout, ","));
    std::cout << v.back() << "\n";
  }
  else
    std::cout << "an error occured.";
}
