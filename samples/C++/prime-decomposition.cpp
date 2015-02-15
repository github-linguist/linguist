#include <iostream>
#include <gmpxx.h>

// This function template works for any type representing integers or
// nonnegative integers, and has the standard operator overloads for
// arithmetic and comparison operators, as well as explicit conversion
// from int.
//
// OutputIterator must be an output iterator with value_type Integer.
// It receives the prime factors.
template<typename Integer, typename OutputIterator>
 void decompose(Integer n, OutputIterator out)
{
  Integer i(2);

  while (n != 1)
  {
    while (n % i == Integer(0))
    {
      *out++ = i;
      n /= i;
    }
    ++i;
  }
}

// this is an output iterator similar to std::ostream_iterator, except
// that it outputs the separation string *before* the value, but not
// before the first value (i.e. it produces an infix notation).
template<typename T> class infix_ostream_iterator:
  public std::iterator<T, std::output_iterator_tag>
{
  class Proxy;
  friend class Proxy;
  class Proxy
  {
  public:
    Proxy(infix_ostream_iterator& iter): iterator(iter) {}
    Proxy& operator=(T const& value)
    {
      if (!iterator.first)
      {
        iterator.stream << iterator.infix;
      }
      iterator.stream << value;
    }
  private:
    infix_ostream_iterator& iterator;
  };
public:
  infix_ostream_iterator(std::ostream& os, char const* inf):
    stream(os),
    first(true),
    infix(inf)
  {
  }
  infix_ostream_iterator& operator++() { first = false; return *this; }
  infix_ostream_iterator operator++(int)
  {
    infix_ostream_iterator prev(*this);
    ++*this;
    return prev;
  }
  Proxy operator*() { return Proxy(*this); }
private:
  std::ostream& stream;
  bool first;
  char const* infix;
};

int main()
{
  std::cout << "please enter a positive number: ";
  mpz_class number;
  std::cin >> number;

  if (number <= 0)
    std::cout << "this number is not positive!\n;";
  else
  {
    std::cout << "decomposition: ";
    decompose(number, infix_ostream_iterator<mpz_class>(std::cout, " * "));
    std::cout << "\n";
  }
}
