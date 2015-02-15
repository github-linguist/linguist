#include <iostream>
#include <time.h>

//------------------------------------------------------------------------------
using namespace std;

//------------------------------------------------------------------------------
const int MAX = 30;

//------------------------------------------------------------------------------
class cSort
{
public:
    void sort( int* arr, int len )
    {
	int mi, mx, z = 0; findMinMax( arr, len, mi, mx );
	int nlen = ( mx - mi ) + 1; int* temp = new int[nlen];
	memset( temp, 0, nlen * sizeof( int ) );

	for( int i = 0; i < len; i++ ) temp[arr[i] - mi]++;

	for( int i = mi; i <= mx; i++ )
	{
	    while( temp[i - mi] )
	    {
		arr[z++] = i;
		temp[i - mi]--;
	    }
	}

	delete [] temp;
    }

private:
    void findMinMax( int* arr, int len, int& mi, int& mx )
    {
	mi = INT_MAX; mx = 0;
	for( int i = 0; i < len; i++ )
	{
	    if( arr[i] > mx ) mx = arr[i];
	    if( arr[i] < mi ) mi = arr[i];
	}
    }
};
//------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    srand( time( NULL ) ); int arr[MAX];
    for( int i = 0; i < MAX; i++ )
	arr[i] = rand() % 140 - rand() % 40 + 1;
	
    for( int i = 0; i < MAX; i++ )
	cout << arr[i] << ", ";
    cout << endl << endl;

    cSort s; s.sort( arr, MAX );

    for( int i = 0; i < MAX; i++ )
	cout << arr[i] << ", ";
    cout << endl << endl;

    return system( "pause" );
}
//------------------------------------------------------------------------------
