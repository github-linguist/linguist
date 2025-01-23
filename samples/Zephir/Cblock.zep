/**
 * CBLOCK tests
 * asfas
 */

%{
// top statement before namespace, add to after headers
#define MAX_FACTOR 40
}%

namespace Test;

%{
// top statement before class, add to after headers
// test include .h
#include "kernel/require.h"
}%

%{

// c implement fibonacci
static long fibonacci(long n) {
        if (n < 2) return n;
        else return fibonacci(n - 2) + fibonacci(n - 1);
}

}%

class Cblock
{
    public function testCblock1()
    {
        int a = 0;

        %{
            a = MAX_FACTOR;
        }%

        return a;
    }

    public function testCblock2()
    {
            long a = 0;

            %{
                a = fibonacci(MAX_FACTOR);
            }%

            return a;
    }

}
