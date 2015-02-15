#include <iostream>
#include <string>

int main( ) {
   std::string original ("This is the original");
   std::string my_copy = original;
   std::cout << "This is the copy: " << my_copy << std::endl;
   original = "Now we change the original! ";
   std::cout << "my_copy still is " << my_copy << std::endl;
}
