#include <string>

// ...

std::string str; // a string object for an empty string

if (str.empty()) { ... } // to test if string is empty

// we could also use the following
if (str.length() == 0) { ... }
if (str == "") { ... }
