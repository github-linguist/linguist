#include <string>
#include <algorithm>
#include <iterator>
#include <cstddef>
#include <exception>
#include <iostream>

// an exception to throw (actually, throwing an exception in this case is generally considered bad style, but it's part of the task)
class not_found: public std::exception
{
public:
  not_found(std::string const& s): text(s + " not found") {}
  char const* what() const throw() { return text.c_str(); }
  ~not_found() throw() {}
private:
  std::string text;
};

// needle search function, C-style interface version using standard library
std::size_t get_index(std::string* haystack, int haystack_size, std::string needle)
{
  std::size_t index = std::find(haystack, haystack+haystack_size, needle) - haystack;
  if (index == haystack_size)
    throw not_found(needle);
  else
    return index;
}

// needle search function, completely generic style, needs forward iterators
// (works with any container, but inefficient if not random-access-iterator)
template<typename FwdIter>
 typename std::iterator_traits<FwdIter>::difference_type fwd_get_index(FwdIter first, FwdIter last, std::string needle)
{
  FwdIter elem = std::find(first, last, needle);
  if (elem == last)
    throw not_found(needle);
  else
    return std::distance(first, elem);
}

// needle search function, implemented directly, needs only input iterator, works efficiently with all sequences
template<typename InIter>
 typename std::iterator_traits<InIter>::difference_type generic_get_index(InIter first, InIter last, std::string needle)
{
  typename std::iterator_traits<InIter>::difference_type index = 0;
  while (first != last && *first != needle)
  {
    ++index;
    ++first;
  }
  if (first == last)
    throw not_found(needle);
  else
    return index;
}

// ----------------------------------------------------------------------------------------------------------------------------------

// a sample haystack (content copied from Haskell example)
std::string haystack[] = { "Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie", "Bush", "Bozo" };

// some useful helper functions
template<typename T, std::size_t sz> T* begin(T (&array)[sz]) { return array; }
template<typename T, std::size_t sz> T* end(T (&array)[sz]) { return array + sz; }
template<typename T, std::size_t sz> std::size_t size(T (&array)[sz]) { return sz; }

// test function searching a given needle with each of the methods
void test(std::string const& needle)
{
  std::cout << "-- C style interface --\n";
  try
  {
    std::size_t index = get_index(haystack, size(haystack), needle);
    std::cout << needle << " found at index " << index << "\n";
  }
  catch(std::exception& exc) // better catch standard exceptions as well; me might e.g. run out of memory
  {
    std::cout << exc.what() << "\n";
  }

  std::cout << "-- generic interface, first version --\n";
  try
  {
    std::size_t index = fwd_get_index(begin(haystack), end(haystack), needle);
    std::cout << needle << " found at index " << index << "\n";
  }
  catch(std::exception& exc) // better catch standard exceptions as well; me might e.g. run out of memory
  {
    std::cout << exc.what() << "\n";
  }

  std::cout << "-- generic interface, second version --\n";
  try
  {
    std::size_t index = generic_get_index(begin(haystack), end(haystack), needle);
    std::cout << needle << " found at index " << index << "\n";
  }
  catch(std::exception& exc) // better catch standard exceptions as well; me might e.g. run out of memory
  {
    std::cout << exc.what() << "\n";
  }
}

int main()
{
  std::cout << "\n=== Word which only occurs once ===\n";
  test("Wally");
  std::cout << "\n=== Word occuring multiple times ===\n";
  test("Bush");
  std::cout << "\n=== Word not occuring at all ===\n";
  test("Goofy");
}
