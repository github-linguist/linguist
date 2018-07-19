#include <ctime>
#include <iostream>
using namespace std;

int identity(int x) { return x; }
int sum(int num) {
  for (int i = 0; i < 1000000; i++)
    num += i;
  return num;
}

double time_it(int (*action)(int), int arg) {
  clock_t start_time = clock();
  action(arg);
  clock_t finis_time = clock();
  return ((double) (finis_time - start_time)) / CLOCKS_PER_SEC;
}

int main() {
  cout << "Identity(4) takes " << time_it(identity, 4) << " seconds." << endl;
  cout << "Sum(4) takes " << time_it(sum, 4) << " seconds." << endl;
  return 0;
}
