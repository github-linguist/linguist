#include "boost/filesystem.hpp"
#include "boost/regex.hpp"
#include <iostream>

using namespace boost::filesystem;

int main()
{
  path current_dir("."); //
  boost::regex pattern("a.*"); // list all files starting with a
  for (recursive_directory_iterator iter(current_dir), end;
       iter != end;
       ++iter)
  {
    std::string name = iter->path().leaf();
    if (regex_match(name, pattern))
      std::cout << iter->path() << "\n";
  }
}
