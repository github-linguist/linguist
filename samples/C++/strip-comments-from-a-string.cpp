#include <iostream>
#include <string>

std::string strip_white(const std::string& input)
{
   size_t b = input.find_first_not_of(' ');
   if (b == std::string::npos) b = 0;
   return input.substr(b, input.find_last_not_of(' ') + 1 - b);
}

std::string strip_comments(const std::string& input, const std::string& delimiters)
{
   return strip_white(input.substr(0, input.find_first_of(delimiters)));
}

int main( ) {
   std::string input;
   std::string delimiters("#;");
   while ( getline(std::cin, input) && !input.empty() ) {
      std::cout << strip_comments(input, delimiters) << std::endl ;
   }
   return 0;
}
