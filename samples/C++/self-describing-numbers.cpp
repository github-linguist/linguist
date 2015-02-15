#include <iostream>

//--------------------------------------------------------------------------------------------------
typedef unsigned long long bigint;

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
class sdn
{
public:
    bool check( bigint n )
    {
	int cc = digitsCount( n );
	return compare( n, cc );
    }

    void displayAll( bigint s )
    {
	for( bigint y = 1; y < s; y++ )
	    if( check( y ) )
		cout << y << " is a Self-Describing Number." << endl;
    }

private:
    bool compare( bigint n, int cc )
    {
	bigint a;
	while( cc )
	{
	    cc--; a = n % 10;
	    if( dig[cc] != a ) return false;
	    n -= a; n /= 10;
	}
	return true;
    }

    int digitsCount( bigint n )
    {
	int cc = 0; bigint a;
	memset( dig, 0, sizeof( dig ) );
	while( n )
	{
	    a = n % 10; dig[a]++;
	    cc++ ; n -= a; n /= 10;
	}
	return cc;
    }

    int dig[10];
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    sdn s;
    s. displayAll( 1000000000000 );
    cout << endl << endl; system( "pause" );

    bigint n;
    while( true )
    {
	system( "cls" );
	cout << "Enter a positive whole number ( 0 to QUIT ): "; cin >> n;
	if( !n ) return 0;
	if( s.check( n ) ) cout << n << " is";
	else cout << n << " is NOT";
	cout << " a Self-Describing Number!" << endl << endl;
	system( "pause" );
    }

    return 0;
}
//--------------------------------------------------------------------------------------------------
