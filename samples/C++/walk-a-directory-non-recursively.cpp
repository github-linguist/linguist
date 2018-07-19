#include "boost/filesystem.hpp"
#include "boost/regex.hpp"
#include <iostream>

using namespace boost::filesystem;

int main()
{
  path current_dir(".");
  // list all files starting with a
  boost::regex pattern("a.*");
  for (directory_iterator iter(current_dir), end;
       iter != end;
       ++iter)
  {
    boost::smatch match;
    std::string fn = iter->path().filename().string(); // must make local variable
    if (boost::regex_match( fn, match, pattern))
    {
      std::cout << match[0] << "\n";
    }
  }
}
