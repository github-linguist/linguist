#include <iostream>
using namespace std;

int main()
{
  int a, b;
  cin >> a >> b;

  // test for less-than
  if (a < b)
    cout << a << " is less than " << b << endl;

  // test for equality
  if (a == b)
    cout << a << " is equal to " << b << endl;

  // test for greater-than
  if (a > b)
    cout << a << " is greater than " << b << endl;
}
