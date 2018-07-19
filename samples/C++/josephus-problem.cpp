#include <iostream>
#include <vector>

//--------------------------------------------------------------------------------------------------
using namespace std;
typedef unsigned long long bigint;

//--------------------------------------------------------------------------------------------------
class josephus
{
public:
    bigint findSurvivors( bigint n, bigint k, bigint s = 0 )
    {
	bigint i = s + 1;
	for( bigint x = i; x <= n; x++, i++ )
	    s = ( s + k ) % i;

	return s;
    }

    void getExecutionList( bigint n, bigint k, bigint s = 1 )
    {
	cout << endl << endl << "Execution list: " << endl;

	prisoners.clear();
	for( bigint x = 0; x < n; x++ )
	    prisoners.push_back( x );

	bigint index = 0;
	while( prisoners.size() > s )
	{
	    index += k - 1;
	    if( index >= prisoners.size() ) index %= prisoners.size();
	    cout << prisoners[static_cast<unsigned int>( index )] << ", ";

	    vector<bigint>::iterator it = prisoners.begin() + static_cast<unsigned int>( index );
	    prisoners.erase( it );
	}
    }

private:
    vector<bigint> prisoners;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    josephus jo;
    bigint n, k, s;
    while( true )
    {
	system( "cls" );
	cout << "Number of prisoners( 0 to QUIT ): "; cin >> n;
	if( !n ) return 0;
	cout << "Execution step: "; cin >> k;
	cout << "How many survivors: "; cin >> s;
		
	cout << endl << "Survivor";
	if( s == 1 )
	{
	    cout << ": " << jo.findSurvivors( n, k );
	    jo.getExecutionList( n, k );
	}
	else
	{
	    cout << "s: ";
	    for( bigint x = 0; x < s; x++ )
		cout << jo.findSurvivors( n, k, x ) << ", ";

	    jo.getExecutionList( n, k, s );
	}

	cout << endl << endl;
	system( "pause" );
    }
    return 0;
}
//--------------------------------------------------------------------------------------------------
