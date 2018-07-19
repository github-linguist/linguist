#include <iterator>
#include <utility>
#include <algorithm>
#include <list>
#include <iostream>

// helper struct
template<typename T> struct referring
{
  referring(T const& t): value(t) {}
  template<typename Iter>
   bool operator()(std::pair<Iter, int> const& p) const
  {
    return *p.first == value;
  }
  T const& value;
};

// requires:
// FwdIterator is a ForwardIterator
// The value_type of FwdIterator is EqualityComparable
// OutIterator is an output iterator
// the value_type of FwdIterator is convertible to the value_type of OutIterator
// [first, last) is a valid range
// provides:
// the mode is written to result
template<typename FwdIterator, typename OutIterator>
 void mode(FwdIterator first, FwdIterator last, OutIterator result)
{
  typedef typename std::iterator_traits<FwdIterator>::value_type value_type;
  typedef std::list<std::pair<FwdIterator, int> > count_type;
  typedef typename count_type::iterator count_iterator;

  // count elements
  count_type counts;

  while (first != last)
  {
    count_iterator element = std::find_if(counts.begin(), counts.end(),
                                          referring<value_type>(*first));
    if (element == counts.end())
      counts.push_back(std::make_pair(first, 1));
    else
      ++element->second;
    ++first;
  }

  // find maximum
  int max = 0;
  for (count_iterator i = counts.begin(); i != counts.end(); ++i)
    if (i->second > max)
      max = i->second;

  // copy corresponding elements to output sequence
  for (count_iterator i = counts.begin(); i != counts.end(); ++i)
    if (i->second == max)
      *result++ = *i->first;
}

// example usage
int main()
{
  int values[] = { 1, 2, 3, 1, 2, 4, 2, 5, 2, 3, 3, 1, 3, 6 };
  median(values, values + sizeof(values)/sizeof(int),
         std::ostream_iterator<int>(std::cout, " "));
  std::cout << std::endl;
  return 0;
}
