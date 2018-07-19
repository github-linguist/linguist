#include <iostream>
#include <algorithm>
#include <string>
#include <iterator>

struct GuessNumberIterator : std::iterator<std::random_access_iterator_tag, int> {
  int i;
  GuessNumberIterator() { }
  GuessNumberIterator(int _i) : i(_i) { }
  GuessNumberIterator& operator++() { ++i; return *this; }
  GuessNumberIterator operator++(int) {
    GuessNumberIterator tmp = *this; ++(*this); return tmp; }
  bool operator==(const GuessNumberIterator& y) { return i == y.i; }
  bool operator!=(const GuessNumberIterator& y) { return i != y.i; }
  int operator*() {
    std::cout << "Is your number less than or equal to " << i << "? ";
    std::string s;
    std::cin >> s;
    return (s != "" && (s[0] == 'y' || s[0] == 'Y')) ? 0 : -1;
  }
  GuessNumberIterator& operator--() { --i; return *this; }
  GuessNumberIterator operator--(int) {
    GuessNumberIterator tmp = *this; --(*this); return tmp; }
  GuessNumberIterator& operator+=(int n) { i += n; return *this; }
  GuessNumberIterator& operator-=(int n) { i -= n; return *this; }
  GuessNumberIterator operator+(int n) {
    GuessNumberIterator tmp = *this; return tmp += n; }
  GuessNumberIterator operator-(int n) {
    GuessNumberIterator tmp = *this; return tmp -= n; }
  int operator-(const GuessNumberIterator &y) { return i - y.i; }
  int operator[](int n) { return *(*this + n); }
  bool operator<(const GuessNumberIterator &y) { return i < y.i; }
  bool operator>(const GuessNumberIterator &y) { return i > y.i; }
  bool operator<=(const GuessNumberIterator &y) { return i <= y.i; }
  bool operator>=(const GuessNumberIterator &y) { return i >= y.i; }
};
inline GuessNumberIterator operator+(int n, GuessNumberIterator &i) { return i + n; }

const int lower = 0;
const int upper = 100;

int main() {
  std::cout << "Instructions:\n"
	    << "Think of integer number from " << lower << " (inclusive) to "
	    << upper << " (exclusive) and\n"
	    << "I will guess it. After each guess, I will ask you if it is less than\n"
	    << "or equal to some number, and you will respond with \"yes\" or \"no\".\n";
  int answer = std::lower_bound(GuessNumberIterator(lower), GuessNumberIterator(upper), 0).i;
  std::cout << "Your number is " << answer << ".\n";
  return 0;
}
