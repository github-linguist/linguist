#include <iostream>
#include "math.h"
#include "boost/rational.hpp"

typedef  boost::rational<int> frac;

bool is_perfect(int c)
{
    frac sum(1, c);
    for (int f = 2;f < sqrt(static_cast<float>(c)); ++f){

        if (c % f == 0) sum += frac(1,f) + frac(1, c/f);
    }
    if (sum.denominator() == 1){
 	return (sum == 1);
    }
    return false;
}

int main()
{
    for (int candidate = 2; candidate < 0x80000; ++candidate){
        if (is_perfect(candidate))
	        std::cout << candidate << " is perfect" << std::endl;
    }
    return 0;
}
