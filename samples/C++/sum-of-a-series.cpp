#include <iostream>

double f(double x);

int main()
{
    unsigned int start = 1;
    unsigned int end = 1000;
    double sum = 0;

    for( unsigned int x = start; x <= end; ++x )
    {
        sum += f(x);
    }
    std::cout << "Sum of f(x) from " << start << " to " << end << " is " << sum << std::endl;
    return 0;
}


double f(double x)
{
    return ( 1.0 / ( x * x ) );
}
