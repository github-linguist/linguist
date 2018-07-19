#include <random>
#include <iostream>
#include <iterator>
#include <algorithm>
using namespace std;

mt19937 engine; //mersenne twister

unsigned int one_of_n(unsigned int n) {
	unsigned int choice;
	for(unsigned int i = 0; i < n; ++i) {
		uniform_int_distribution<unsigned int> distribution(0, i);
		if(!distribution(engine))
			choice = i;
	}
	return choice;
}

int main() {
	engine = mt19937(random_device()()); //seed random generator from system
	unsigned int results[10] = {0};
	for(unsigned int i = 0; i < 1000000; ++i)
		results[one_of_n(10)]++;
	ostream_iterator<unsigned int> out_it(cout, " ");
	copy(results, results+10, out_it);
	cout << '\n';
}
