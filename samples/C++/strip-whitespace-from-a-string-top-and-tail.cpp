#include <boost/algorithm/string.hpp>
#include <string>
#include <iostream>

int main( ) {
   std::string testphrase( "    There are unwanted blanks here!    " ) ;
   std::string lefttrimmed = boost::trim_left_copy( testphrase ) ;
   std::string righttrimmed = boost::trim_right_copy( testphrase ) ;
   std::cout << "The test phrase is :" << testphrase << "\n" ;
   std::cout << "Trimmed on the left side :" << lefttrimmed << "\n" ;
   std::cout << "Trimmed on the right side :" << righttrimmed << "\n" ;
   boost::trim( testphrase ) ;
   std::cout << "Trimmed on both sides :" <<  testphrase  << "\n" ;
   return 0 ;
}
