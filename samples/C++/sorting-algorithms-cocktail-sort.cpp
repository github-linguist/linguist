#include <iostream>
#include <windows.h>

//--------------------------------------------------------------------------------------------------
const int EL_COUNT = 77, LLEN = 11;

//--------------------------------------------------------------------------------------------------
class cocktailSort
{
public:
    void sort( int* arr, int len )
    {
	bool notSorted = true;
	while( notSorted )
	{
	    notSorted = false;
	    for( int a = 0; a < len - 1; a++ )
	    {
		if( arr[a] > arr[a + 1] )
		{
		    sSwap( arr[a], arr[a + 1] );
		    notSorted = true;
		}
	    }

	    if( !notSorted ) break;
	    notSorted = false;

	    for( int a = len - 1; a > 0; a-- )
	    {
		if( arr[a - 1] > arr[a] )
		{
		    sSwap( arr[a], arr[a - 1] );
		    notSorted = true;
		}
	    }
	}
    }

private:
    void sSwap( int& a, int& b )
    {
	int t = a;
   	a = b; b = t;
    }
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    srand( GetTickCount() );
    cocktailSort cs;
    int arr[EL_COUNT];
	
    for( int x = 0; x < EL_COUNT; x++ )
        arr[x] = rand() % EL_COUNT + 1;

    std::cout << "Original: " << std::endl << "==========" << std::endl;
    for( int x = 0; x < EL_COUNT; x += LLEN )
    {
	for( int s = x; s < x + LLEN; s++ )
	    std::cout << arr[s] << ", ";

	std::cout << std::endl;
    }

    //DWORD now = GetTickCount();
    cs.sort( arr, EL_COUNT );
    //now = GetTickCount() - now;
	
    std::cout << std::endl << std::endl << "Sorted: " << std::endl << "========" << std::endl;
    for( int x = 0; x < EL_COUNT; x += LLEN )
    {
	for( int s = x; s < x + LLEN; s++ )
	    std::cout << arr[s] << ", ";

	std::cout << std::endl;
    }

    std::cout << std::endl << std::endl << std::endl << std::endl;
    //std::cout << now << std::endl << std::endl;
    system( "pause" );

    return 0;
}
//--------------------------------------------------------------------------------------------------
