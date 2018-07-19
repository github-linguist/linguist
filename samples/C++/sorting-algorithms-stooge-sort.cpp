#include <iostream>
#include <time.h>

//------------------------------------------------------------------------------
using namespace std;

//------------------------------------------------------------------------------
class stooge
{
public:
    void sort( int* arr, int start, int end )
    {
        if( arr[start] > arr[end - 1] ) swap( arr[start], arr[end - 1] );
	int n = end - start; if( n > 2 )
	{
	    n /= 3; sort( arr, start, end - n );
	    sort( arr, start + n, end ); sort( arr, start, end - n );
        }
    }
};
//------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    srand( static_cast<unsigned int>( time( NULL ) ) ); stooge s; int a[80], m = 80;
    cout << "before:\n";
    for( int x = 0; x < m; x++ ) { a[x] = rand() % 40 - 20;  cout << a[x] << " "; }
    s.sort( a, 0, m ); cout << "\n\nafter:\n";
    for( int x = 0; x < m; x++ ) cout << a[x] << " "; cout << "\n\n";
    return system( "pause" );
}
