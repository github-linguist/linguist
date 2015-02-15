#include <iostream>
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <vector>
#include <time.h>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const unsigned int LEN = 4;

//--------------------------------------------------------------------------------------------------
class CowsAndBulls_Player
{
public:
    CowsAndBulls_Player() { fillPool(); }
    void play() { secret = createSecret(); guess(); }

private:
    void guess()
    {
	pair<int, int> res; int cc = 1;
	cout << endl << " SECRET: " << secret << endl << "==============" << endl;
	cout << "+-----------+---------+--------+\n|   GUESS   |  BULLS  |  COWS  |\n+-----------+---------+--------+\n";
	while( true )
	{
	    string gs = gimmeANumber();
	    if( gs.empty() ) { cout << endl << "Something went wrong with the scoring..." << endl << "Cannot find an answer!" << endl; return; }
	    if( scoreIt( gs, res ) ) { cout << endl << "I found the secret number!" << endl << "It is: " << gs << endl; return; }
	    cout << "|    " << gs << "   |  " << setw( 3 ) << res.first << "    |  " << setw( 3 ) << res.second << "   |\n+-----------+---------+--------+\n";
	    clearPool( gs, res );
        }
    }

    void clearPool( string gs, pair<int, int>& r )
    {
	vector<string>::iterator pi = pool.begin();
	while( pi != pool.end() )
	{
	    if( removeIt( gs, ( *pi ), r ) ) pi = pool.erase( pi );
	    else  pi++;
	}
    }

    string gimmeANumber()
    {
	if( pool.empty() ) return "";
	return pool[rand() % pool.size()];
    }

    void fillPool()
    {
	for( int x = 1234; x < 9877; x++ )
	{
	    ostringstream oss; oss << x;
	    if( check( oss.str() ) ) pool.push_back( oss.str() );
	}
    }

    bool check( string s )
    {
	for( string::iterator si = s.begin(); si != s.end(); si++ )
	{
	    if( ( *si ) == '0' ) return false;
	    if( count( s.begin(), s.end(), ( *si ) ) > 1 ) return false;
	}
	return true;
    }

    bool removeIt( string gs, string ts, pair<int, int>& res )
    {
	pair<int, int> tp; getScore( gs, ts, tp );
	return tp != res;
    }

    bool scoreIt( string gs, pair<int, int>& res )
    {
	getScore( gs, secret, res );
	return res.first == LEN;
    }

    void getScore( string gs, string st, pair<int, int>& pr )
    {
	pr.first = pr.second = 0;
	for( unsigned int ui = 0; ui < LEN; ui++ )
	{
	    if( gs[ui] == st[ui] ) pr.first++;
	    else
	    {
		for( unsigned int vi = 0; vi < LEN; vi++ )
		    if( gs[ui] == st[vi] ) pr.second++;
	    }
	}
    }

    string createSecret()
    {
	string n = "123456789", rs = "";
	while( rs.length() < LEN )
	{
	    int r = rand() % n.length();
	    rs += n[r]; n.erase( r, 1 );
	}
	return rs;
    }

    string secret;
    vector<string> pool;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    srand( static_cast<unsigned int>( time( NULL ) ) ); CowsAndBulls_Player cb;
    cb.play(); cout << endl << endl;
    return system( "pause" );
}
//--------------------------------------------------------------------------------------------------
