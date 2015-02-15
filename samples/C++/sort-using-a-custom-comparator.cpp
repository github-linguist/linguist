#include <algorithm>
#include <string>
#include <cctype>

// compare character case-insensitive
struct icompare_char {
  bool operator()(char c1, char c2) {
    return std::toupper(c1) < std::toupper(c2);
  }
};

// return true if s1 comes before s2
struct compare {
  bool operator()(std::string const& s1, std::string const& s2) {
    if (s1.length() > s2.length())
      return true;
    if (s1.length() < s2.length())
      return false;
    return std::lexicographical_compare(s1.begin(), s1.end(),
                                        s2.begin(), s2.end(),
                                        icompare_char());
  }
};

int main() {
  std::string strings[8] = {"Here", "are", "some", "sample", "strings", "to", "be", "sorted"};
  std::sort(strings, strings+8, compare());
  return 0;
}
