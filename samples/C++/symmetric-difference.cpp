#include <iostream>
#include <set>
#include <algorithm>
#include <iterator>
#include <string>

using namespace std;

int main( ) {
   string setA[] = { "John", "Bob" , "Mary", "Serena" };
   string setB[] = { "Jim" , "Mary", "John", "Bob"  };
   set<string>
       firstSet( setA , setA + 4 ),
       secondSet( setB , setB + 4 ),
       symdiff;

   set_symmetric_difference( firstSet.begin(), firstSet.end(),
                             secondSet.begin(), secondSet.end(),
                             inserter( symdiff, symdiff.end() ) );

   copy( symdiff.begin(), symdiff.end(), ostream_iterator<string>( cout , " " ) );
   cout << endl;
   return 0;
}
