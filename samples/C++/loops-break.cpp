#include <iostream>
#include <ctime>
#include <cstdlib>

int main()
{
    srand(time(0));
    while(true)
    {
    	int a = 0 + rand() % 19;
    	std::cout << a << std::endl;
    	if (a == 10)
    		break;
    	int b = 0 + rand() % 19;
    	std::cout << b << std::endl;
    }
    return 0;
}
