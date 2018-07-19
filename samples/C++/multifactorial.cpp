#include <algorithm>
#include <iostream>
#include <iterator>
/*Generate multifactorials to 9

  Nigel_Galloway
  November 14th., 2012.
*/
int main(void) {
   for (int g = 1; g < 10; g++) {
     int v[11], n=0;
     generate_n(std::ostream_iterator<int>(std::cout, " "), 10, [&]{n++; return v[n]=(g<n)? v[n-g]*n : n;});
     std::cout << std::endl;
   }
   return 0;
}
