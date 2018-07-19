#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <vector>
#include <algorithm>
#include <iterator>

int main() {
  std::ifstream in("unixdict.txt");
  typedef  std::map<std::string, std::vector<std::string> > AnagramMap;
  AnagramMap anagrams;

  std::string word;
  size_t count = 0;
  while (std::getline(in, word)) {
    std::string key = word;
    std::sort(key.begin(), key.end());
    // note: the [] op. automatically inserts a new value if key does not exist
    AnagramMap::mapped_type & v = anagrams[key];
    v.push_back(word);
    count = std::max(count, v.size());
  }

  in.close();

  for (AnagramMap::const_iterator it = anagrams.begin(), e = anagrams.end();
       it != e; it++)
    if (it->second.size() >= count) {
      std::copy(it->second.begin(), it->second.end(),
                std::ostream_iterator<std::string>(std::cout, ", "));
      std::cout << std::endl;
    }
  return 0;
}
