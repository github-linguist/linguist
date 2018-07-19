#include <iostream>

double f(double x)
{
	return (x*x*x - 3*x*x + 2*x);
}

int main()
{
	double step = 0.001; // Smaller step values produce more accurate and precise results
	double start = -1;
	double stop = 3;
	double value = f(start);
	double sign = (value > 0);
	
	// Check for root at start
	if ( 0 == value )
		std::cout << "Root found at " << start << std::endl;

	for(	double x = start + step;
			x <= stop;
			x += step )
	{
		value = f(x);
		
		if ( ( value > 0 ) != sign )
			// We passed a root
			std::cout << "Root found near " << x << std::endl;
		else if ( 0 == value )
			// We hit a root
			std::cout << "Root found at " << x << std::endl;
		
		// Update our sign
		sign = ( value > 0 );
	}
}
