#include <algorithm>
#include <cctype>
#include <string>
using namespace std;

const string alphabet("abcdefghijklmnopqrstuvwxyz"); // sorted, no duplicates

bool is_pangram(string s) {
  // Convert to lower case.
  transform(s.begin(), s.end(), s.begin(), ::tolower);
  // Convert to a sorted sequence of (not necessarily unique) characters.
  sort(s.begin(), s.end());
  // Is the second sequence a subset of the first sequence?
  // Repeated letters in "s" are okay, since it still "includes" the single letter
  return includes(s.begin(), s.end(), alphabet.begin(), alphabet.end());
}
