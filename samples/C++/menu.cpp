#include <iostream>
#include <boost/regex.hpp>
#include <cstdlib>
#include <string>
using namespace std;

void printMenu(const string *, int);
//checks whether entered data is in required range
bool checkEntry(string, const string *, int);

string dataEntry(string prompt, const string *terms, int size) {
   if (size == 0) { //we return an empty string when we call the function with an empty list
      return "";
   }

   string entry;
   do {
      printMenu(terms, size);
      cout << prompt;

      cin >> entry;
   }
   while( !checkEntry(entry, terms, size) );

   int number = atoi(entry.c_str());
   return terms[number - 1];
}

void printMenu(const string *terms, int num) {
   for (int i = 1 ; i < num + 1 ; i++) {
      cout << i << ')' << terms[ i - 1 ] << '\n';
   }
}

bool checkEntry(string myEntry, const string *terms, int num) {
   boost::regex e("^\\d+$");
   if (!boost::regex_match(myEntry, e))
      return false;
   int number = atoi(myEntry.c_str());
   if (number < 1 || number > num)
      return false;
   return true;
}

int main( ) {
   const string terms[ ] = { "fee fie" , "huff and puff" , "mirror mirror" , "tick tock" };
   int size = sizeof terms / sizeof *terms;
   cout << "You chose: " << dataEntry("Which is from the three pigs: ", terms, size);
   return 0;
}
