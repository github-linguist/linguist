#include <time.h>
#include <iostream>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const int MAX = 126;
class shell
{
public:
    shell()
    { _gap[0] = 1750; _gap[1] = 701; _gap[2] = 301; _gap[3] = 132; _gap[4] = 57; _gap[5] = 23; _gap[6] = 10; _gap[7] = 4; _gap[8] = 1; }

    void sort( int* a, int count )
    {
	_cnt = count;
	for( int x = 0; x < 9; x++ )
	    if( count > _gap[x] )
	    { _idx = x; break; }

	sortIt( a );
    }

private:	
    void sortIt( int* arr )
    {
	bool sorted = false;
	while( true )
	{
	    sorted = true;
	    int st = 0;
	    for( int x = _gap[_idx]; x < _cnt; x += _gap[_idx] )
	    {
		if( arr[st] > arr[x] )
		{ swap( arr[st], arr[x] ); sorted = false; }
		st = x;
	    }
	    if( ++_idx >= 8 ) _idx = 8;
	    if( sorted && _idx == 8 ) break;
	}
    }

    void swap( int& a, int& b ) { int t = a; a = b; b = t; }

    int _gap[9], _idx, _cnt;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    srand( static_cast<unsigned int>( time( NULL ) ) ); int arr[MAX];
    for( int x = 0; x < MAX; x++ )
	arr[x] = rand() % MAX - rand() % MAX;

    cout << " Before: \n=========\n";
    for( int x = 0; x < 7; x++ )
    {
	for( int a = 0; a < 18; a++ )
	{ cout << arr[x * 18 + a] << " "; }
	cout << endl;
    }
    cout << endl; shell s; s.sort( arr, MAX );
	
    cout << " After: \n========\n";
    for( int x = 0; x < 7; x++ )
    {
	for( int a = 0; a < 18; a++ )
	{ cout << arr[x * 18 + a] << " "; }
	cout << endl;
    }
    cout << endl << endl; return system( "pause" );
}
//--------------------------------------------------------------------------------------------------
