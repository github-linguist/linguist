#include <windows.h>
#include <iostream>
#include <string>

//-------------------------------------------------------------------------------
using namespace std;

//-------------------------------------------------------------------------------
enum choices { ROCK, SPOCK, PAPER, LIZARD, SCISSORS, MX_C };
enum indexes { PLAYER, COMPUTER, DRAW };

//-------------------------------------------------------------------------------
class stats
{
public:
    stats() : _draw( 0 )
    {
        ZeroMemory( _moves, sizeof( _moves ) );
	ZeroMemory( _win, sizeof( _win ) );
    }
    void draw()		        { _draw++; }
    void win( int p )	        { _win[p]++; }
    void move( int p, int m )   { _moves[p][m]++; }
    int getMove( int p, int m ) { return _moves[p][m]; }
    string format( int a )
    {
	char t[32];
	wsprintf( t, "%.3d", a );
	string d( t );
	return d;
    }

    void print()
    {
        string  d = format( _draw ),
	       pw = format( _win[PLAYER] ),		cw = format( _win[COMPUTER] ),
	       pr = format( _moves[PLAYER][ROCK] ),	cr = format( _moves[COMPUTER][ROCK] ),
               pp = format( _moves[PLAYER][PAPER] ),	cp = format( _moves[COMPUTER][PAPER] ),
	       ps = format( _moves[PLAYER][SCISSORS] ), cs = format( _moves[COMPUTER][SCISSORS] ),
	       pl = format( _moves[PLAYER][LIZARD] ),	cl = format( _moves[COMPUTER][LIZARD] ),
	       pk = format( _moves[PLAYER][SPOCK] ),	ck = format( _moves[COMPUTER][SPOCK] );

	system( "cls" );
	cout << endl;
	cout << "+----------+-------+--------+--------+---------+----------+--------+---------+" << endl;
	cout << "|          |  WON  |  DRAW  |  ROCK  |  PAPER  | SCISSORS | LIZARD |  SPOCK  |" << endl;
	cout << "+----------+-------+--------+--------+---------+----------+--------+---------+" << endl;
	cout << "|  PLAYER  |  "  << pw << "  |        |   " << pr << "  |   " << pp << "   |   " << ps << "    |  " << pl << "   |   " << pk << "   |" << endl;
	cout << "+----------+-------+   " << d << "  +--------+---------+----------+--------+---------+" << endl;
	cout << "| COMPUTER |  "  << cw << "  |        |   " << cr << "  |   " << cp << "   |   " << cs << "    |  " << cl << "   |   " << ck << "   |" << endl;
	cout << "+----------+-------+--------+--------+---------+----------+--------+---------+" << endl;
	cout << endl << endl;

	system( "pause" );

    }

private:
    int _moves[2][MX_C], _win[2], _draw;
};
//-------------------------------------------------------------------------------
class rps
{
private:
    int makeMove()
    {
	int total = 0, r, s;
	for( int i = 0; i < MX_C; total += statistics.getMove( PLAYER, i++ ) );
	r = rand() % total;

	for( int i = ROCK; i < SCISSORS; i++ )
	{
	    s = statistics.getMove( PLAYER, i );
	    if( r < s ) return ( i + 1 );
	    r -= s;
	}

	return ROCK;
    }

    void printMove( int p, int m )
    {
	if( p == COMPUTER ) cout << "My move: ";
	else cout << "Your move: ";

	switch( m )
	{
	    case ROCK: cout << "ROCK\n"; break;
	    case PAPER: cout << "PAPER\n"; break;
	    case SCISSORS: cout << "SCISSORS\n"; break;
	    case LIZARD: cout << "LIZARD\n"; break;
	    case SPOCK: cout << "SPOCK\n";
	}
    }

public:
    rps()
    {
	checker[ROCK][ROCK] = 2; checker[ROCK][PAPER] = 1; checker[ROCK][SCISSORS] = 0; checker[ROCK][LIZARD] = 0; checker[ROCK][SPOCK] = 1;
	checker[PAPER][ROCK] = 0; checker[PAPER][PAPER] = 2; checker[PAPER][SCISSORS] = 1; checker[PAPER][LIZARD] = 1; checker[PAPER][SPOCK] = 0;
	checker[SCISSORS][ROCK] = 1; checker[SCISSORS][PAPER] = 0; checker[SCISSORS][SCISSORS] = 2; checker[SCISSORS][LIZARD] = 0; checker[SCISSORS][SPOCK] = 1;
	checker[LIZARD][ROCK] = 1; checker[LIZARD][PAPER] = 0; checker[LIZARD][SCISSORS] = 1; checker[LIZARD][LIZARD] = 2; checker[LIZARD][SPOCK] = 0;
	checker[SPOCK][ROCK] = 0; checker[SPOCK][PAPER] = 1; checker[SPOCK][SCISSORS] = 0; checker[SPOCK][LIZARD] = 1; checker[SPOCK][SPOCK] = 2;
    }
    void play()
    {
	int p, r, m;
	while( true )
	{
	    cout << "What is your move (1)ROCK (2)SPOCK (3)PAPER (4)LIZARD (5)SCISSORS (0)Quit ? ";
	    cin >> p;
	    if( !p || p < 0 ) break;
	    if( p > 0 && p < 6 )
	    {
		p--;
		cout << endl;
		printMove( PLAYER, p );
		statistics.move( PLAYER, p );

		m = makeMove();
		statistics.move( COMPUTER, m );
		printMove( COMPUTER, m );

		r = checker[p][m];
		switch( r )
		{
		    case DRAW:
		        cout << endl << "DRAW!" << endl << endl;
		        statistics.draw();
		    break;
		    case COMPUTER:
			cout << endl << "I WIN!" << endl << endl;
			statistics.win( COMPUTER );
		    break;
		    case PLAYER:
			cout << endl << "YOU WIN!" << endl << endl;
			statistics.win( PLAYER );

		}
		system( "pause" );
	    }
	    system( "cls" );
	}
	statistics.print();
    }

private:
    stats statistics;
    int checker[MX_C][MX_C];
};
//-------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    srand( GetTickCount() );
    rps game;
    game.play();
    return 0;
}
//-------------------------------------------------------------------------------
