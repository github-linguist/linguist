#include <iostream>
#include <algorithm>
#include <vector>
#include <sstream>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
class myTuple
{
public:
    void set( int a, int b, string c ) { t.first.first = a; t.first.second = b; t.second = c; }
    bool operator == ( pair<int, int> p ) { return p.first == t.first.first && p.second == t.first.second; }
    string second(){ return t.second; }
private:
    pair<pair<int, int>, string> t;
};
//--------------------------------------------------------------------------------------------------
class discordian
{
public:
    discordian()
    {
	myTuple t;
	t.set( 5, 1, "Mungday" ); holyday.push_back( t ); t.set( 19, 2, "Chaoflux" ); holyday.push_back( t );
	t.set( 29, 2, "St. Tib's Day" ); holyday.push_back( t ); t.set( 19, 3, "Mojoday" ); holyday.push_back( t );
	t.set( 3, 5, "Discoflux" ); holyday.push_back( t ); t.set( 31, 5, "Syaday" ); holyday.push_back( t );
	t.set( 15, 7, "Confuflux" ); holyday.push_back( t ); t.set( 12, 8, "Zaraday" ); holyday.push_back( t );
	t.set( 26, 9, "Bureflux" ); holyday.push_back( t ); t.set( 24, 10, "Maladay" ); holyday.push_back( t );
	t.set( 8, 12, "Afflux" ); holyday.push_back( t );
	seasons.push_back( "Chaos" ); seasons.push_back( "Discord" ); seasons.push_back( "Confusion" );
	seasons.push_back( "Bureaucracy" ); seasons.push_back( "The Aftermath" );
	wdays.push_back( "Setting Orange" ); wdays.push_back( "Sweetmorn" ); wdays.push_back( "Boomtime" );
	wdays.push_back( "Pungenday" ); wdays.push_back( "Prickle-Prickle" );
    }

    void convert( int d, int m, int y )
    {
	if( d == 0 || m == 0 || m > 12 || d > getMaxDay( m, y ) ) { cout << "\nThis is not a date!"; return; }
	pair <int, int> p; p.first = d, p.second = m; vector<myTuple>::iterator f;
	f = find( holyday.begin(), holyday.end(), p ); int dd = 0, day, wday, mon,  yr = y +  1166;
	if( d == 29 && m == 2 && isLeap( y ) )
	{ cout << ( *f ).second() << ", Year of Our Lady of Discord " << yr; return; }
	for( int x = 1; x < m; x++ ) dd += getMaxDay( x, 1 ); dd += d; day = dd % 73; wday = dd % 5; mon = dd / 73;
	cout << wdays[wday] << ", " << seasons[mon] << " " << day << ", Year of Our Lady of Discord " << yr;
	if( f != holyday.end() ) cout << " - " << ( *f ).second();
    }

private:
    int getMaxDay( int m, int y )
    { int dd[] = { 0, 31, isLeap( y ) ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }; return dd[m]; }

    bool isLeap( int y )
    { bool l = false; if( !( y % 4 ) ) { if( y % 100 ) l = true; else if( !( y % 400 ) ) l = true; } return l; }

    vector<myTuple> holyday; vector<string> seasons, wdays;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    string date; discordian disc;
    while( true )
    {
	cout << "Enter a date (dd mm yyyy) or 0 to quit: "; getline( cin, date ); if( date == "0" ) break;
	if( date.length() == 10 )
	{
	    istringstream iss( date ); vector<string> vc;
	    copy( istream_iterator<string>( iss ), istream_iterator<string>(), back_inserter<vector<string> >( vc ) );
	    disc.convert( atoi( vc[0].c_str() ), atoi( vc[1].c_str() ), atoi( vc[2].c_str() ) ); cout << "\n\n\n";
	}
	else cout << "\nIs this a date?!\n\n";
    }
    return 0;
}
//--------------------------------------------------------------------------------------------------
