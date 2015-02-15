#include <vector>
#include <string>
#include <iostream>
#include <algorithm>
#include <fstream>
#include <iomanip>
//--------------------------------------------------------------------------------------------------
typedef unsigned int uint;
using namespace std;
const uint TAPE_MAX_LEN = 49152;
//--------------------------------------------------------------------------------------------------
struct action { char write, direction; };
//--------------------------------------------------------------------------------------------------
class tape
{
public:
    tape( uint startPos = TAPE_MAX_LEN >> 1 ) : MAX_LEN( TAPE_MAX_LEN ) { _sp = startPos; reset(); }
    void reset() { clear( '0' ); headPos = _sp; }
    char read(){ return _t[headPos]; }
    void input( string a ){ if( a == "" ) return; for( uint s = 0; s < a.length(); s++ ) _t[headPos + s] = a[s]; }
    void clear( char c ) {  _t.clear(); blk = c; _t.resize( MAX_LEN, blk ); }
    void action( const action* a ) { write( a->write ); move( a->direction ); }
    void print( int c = 10 )
    {
	int ml = static_cast<int>( MAX_LEN ), st = static_cast<int>( headPos ) - c, ed = static_cast<int>( headPos ) + c + 1, tx;
	for( int x = st; x < ed; x++ )
	{ tx = x; if( tx < 0 ) tx += ml; if( tx >= ml ) tx -= ml; cout << _t[tx]; }
	cout << endl << setw( c + 1 ) << "^" << endl;
    }
private:
    void move( char d ) { if( d == 'N' ) return; headPos += d == 'R' ? 1 : -1; if( headPos >= MAX_LEN ) headPos = d == 'R' ? 0 : MAX_LEN - 1; }
    void write( char a ) { if( a != 'N' ) { if( a == 'B' ) _t[headPos] = blk; else _t[headPos] = a; } }
    string _t; uint headPos, _sp; char blk; const uint MAX_LEN;
};
//--------------------------------------------------------------------------------------------------
class state
{
public:
    bool operator ==( const string o ) { return o == name; }
    string name, next; char symbol, write, direction;
};
//--------------------------------------------------------------------------------------------------
class actionTable
{
public:
    bool loadTable( string file )
    {
	reset();
	ifstream mf; mf.open( file.c_str() ); if( mf.is_open() )
	{
	    string str; state stt;
	    while( mf.good() )
	    {
		getline( mf, str ); if( str[0] == '\'' ) break;
		parseState( str, stt ); states.push_back( stt );
	    }
	    while( mf.good() )
	    {
		getline( mf, str ); if( str == "" ) continue;
		if( str[0] == '!' ) blank = str.erase( 0, 1 )[0];
		if( str[0] == '^' ) curState = str.erase( 0, 1 );
		if( str[0] == '>' ) input = str.erase( 0, 1 );
	    }
	    mf.close(); return true;
	}
	cout << "Could not open " << file << endl; return false;
    }

    bool action( char symbol, action& a )
    {
	vector<state>::iterator f = states.begin();
	while( true )
	{
	    f = find( f, states.end(), curState );
	    if( f == states.end() ) return false;
	    if( ( *f ).symbol == '*' || ( *f ).symbol == symbol || ( ( *f ).symbol == 'B' && blank == symbol ) )
	    { a.direction = ( *f ).direction; a.write = ( *f ).write; curState = ( *f ).next; break; }
	    f++;
	}
	return true;
    }
    void reset() { states.clear(); blank = '0'; curState = input = ""; }
    string getInput() { return input; }
    char getBlank() { return blank; }
private:
    void parseState( string str, state& stt )
    {
	string a[5]; int idx = 0;
	for( string::iterator si = str.begin(); si != str.end(); si++ )
	{ if( ( *si ) == ';' ) idx++; else a[idx].append( &( *si ), 1 ); }
	stt.name = a[0]; stt.symbol = a[1][0]; stt.write = a[2][0]; stt.direction = a[3][0]; stt.next = a[4];
    }
    vector<state> states; char blank; string curState, input;
};
//--------------------------------------------------------------------------------------------------
class utm
{
public:
    utm() { files[0] = "incrementer.utm"; files[1] = "busy_beaver.utm"; files[2] = "sort.utm"; }
    void start()
    {
	while( true )
	{
	    reset(); int t = showMenu(); if( t == 0 ) return;
	    if( !at.loadTable( files[t - 1] ) ) return; startMachine();
	}
    }
private:
    void simulate()
    {
	char r; action a;
	while( true ) { tp.print(); r = tp.read(); if( !( at.action( r, a ) ) ) break; tp.action( &a ); }
	cout << endl << endl; system( "pause" );
    }

    int showMenu()
    {
	int t = -1;
	while( t < 0 || t > 3 )
	{
	    system( "cls" ); cout << "1. Incrementer\n2. Busy beaver\n3. Sort\n\n0. Quit";
	    cout << endl << endl << "Choose an action "; cin >> t;
	}
	return t;
    }

    void reset() { tp.reset(); at.reset(); }
    void startMachine() { system( "cls" ); tp.clear( at.getBlank() ); tp.input( at.getInput() ); simulate(); }

    tape tp; actionTable at; string files[7];
};
//--------------------------------------------------------------------------------------------------
int main( int a, char* args[] ){ utm mm; mm.start(); return 0; }
//--------------------------------------------------------------------------------------------------
