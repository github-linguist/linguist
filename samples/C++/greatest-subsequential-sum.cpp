#include <utility>   // for std::pair
#include <iterator>  // for std::iterator_traits
#include <iostream>  // for std::cout
#include <ostream>   // for output operator and std::endl
#include <algorithm> // for std::copy
#include <iterator>  // for std::output_iterator

// Function template max_subseq
//
// Given a sequence of integers, find a subsequence which maximizes
// the sum of its elements, that is, the elements of no other single
// subsequence add up to a value larger than this one.
//
// Requirements:
// * ForwardIterator is a forward iterator
// * ForwardIterator's value_type is less-than comparable and addable
// * default-construction of value_type gives the neutral element
//   (zero)
// * operator+ and operator< are compatible (i.e. if a>zero and
//   b>zero, then a+b>zero, and if a<zero and b<zero, then a+b<zero)
// * [begin,end) is a valid range
//
// Returns:
//   a pair of iterators describing the begin and end of the
//   subsequence
template<typename ForwardIterator>
 std::pair<ForwardIterator, ForwardIterator>
 max_subseq(ForwardIterator begin, ForwardIterator end)
{
  typedef typename std::iterator_traits<ForwardIterator>::value_type
    value_type;

  ForwardIterator seq_begin = begin, seq_end = seq_begin;
  value_type seq_sum = value_type();
  ForwardIterator current_begin = begin;
  value_type current_sum = value_type();

  value_type zero = value_type();

  for (ForwardIterator iter = begin; iter != end; ++iter)
  {
    value_type value = *iter;
    if (zero < value)
    {
      if (current_sum < zero)
      {
        current_sum = zero;
        current_begin = iter;
      }
    }
    else
    {
      if (seq_sum < current_sum)
      {
        seq_begin = current_begin;
        seq_end = iter;
        seq_sum = current_sum;
      }
    }
    current_sum += value;
  }

  if (seq_sum < current_sum)
  {
    seq_begin = current_begin;
    seq_end = end;
    seq_sum = current_sum;
  }

  return std::make_pair(seq_begin, seq_end);
}

// the test array
int array[] = { -1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1 };

// function template to find the one-past-end pointer to the array
template<typename T, int N> int* end(T (&arr)[N]) { return arr+N; }

int main()
{
  // find the subsequence
  std::pair<int*, int*> seq = max_subseq(array, end(array));

  // output it
  std::copy(seq.first, seq.second, std::ostream_iterator<int>(std::cout, " "));
  std::cout << std::endl;

  return 0;
}
