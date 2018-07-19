#include <iostream>

using std::cerr;
using std::endl;

int main () {
  cerr << "Goodbye, World!" << endl;

  return cerr.bad();
}
