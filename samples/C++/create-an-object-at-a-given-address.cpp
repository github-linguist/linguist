#include <string>
#include <iostream>

int main()
{
    // Allocate enough memory to hold an instance of std::string
    char* data = new char[sizeof(std::string)];

    // use placement new to construct a std::string in the memory we allocated previously
    std::string* stringPtr = new (data) std::string("ABCD");

    std::cout << *stringPtr << " 0x" << stringPtr << std::endl;

    // use placement new to construct a new string object in the same memory location
    // remember to manually call destructor
    stringPtr->~basic_string();
    stringPtr = new (data) std::string("123456");

    std::cout << *stringPtr << " 0x" << stringPtr << std::endl;

    // clean up
    stringPtr->~basic_string();
    delete[] data;
}
