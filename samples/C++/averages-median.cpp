#include <algorithm>

// inputs must be random-access iterators of doubles
// Note: this function modifies the input range
template <typename Iterator>
double median(Iterator begin, Iterator end) {
  // this is middle for odd-length, and "upper-middle" for even length
  Iterator middle = begin + (end - begin) / 2;

  // This function runs in O(n) on average, according to the standard
  std::nth_element(begin, middle, end);

  if ((end - begin) % 2 != 0) { // odd length
    return *middle;
  } else { // even length
    // the "lower middle" is the max of the lower half
    Iterator lower_middle = std::max_element(begin, middle);
    return (*middle + *lower_middle) / 2.0;
  }
}

#include <iostream>

int main() {
  double a[] = {4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2};
  double b[] = {4.1, 7.2, 1.7, 9.3, 4.4, 3.2};

  std::cout << median(a+0, a + sizeof(a)/sizeof(a[0])) << std::endl; // 4.4
  std::cout << median(b+0, b + sizeof(b)/sizeof(b[0])) << std::endl; // 4.25

  return 0;
}
