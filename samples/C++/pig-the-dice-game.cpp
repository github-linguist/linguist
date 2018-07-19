#include <windows.h>
#include <iostream>
#include <string>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const int PLAYERS = 2, MAX_POINTS = 100;

//--------------------------------------------------------------------------------------------------
class player
{
public:
    player() { reset(); }
    void reset()
    {
	name = "";
	current_score = round_score = 0;
    }
    string getName()             { return name; }
    void setName( string n )     { name = n; }
    int getCurrScore()           { return current_score; }
    void addCurrScore()          { current_score += round_score; }
    int getRoundScore()          { return round_score; }
    void addRoundScore( int rs ) { round_score += rs; }
    void zeroRoundScore()        { round_score = 0; }

private:
    string name;
    int current_score, round_score;
};
//--------------------------------------------------------------------------------------------------
class pigGame
{
public:
    pigGame() { resetPlayers(); }

    void play()
    {
	while( true )
	{
	    system( "cls" );
	    int p = 0;
	    while( true )
	    {
		if( turn( p ) )
		{
		    praise( p );
		    break;
		}

		++p %= PLAYERS;
	    }

	    string r;
	    cout << "Do you want to play again ( y / n )? "; cin >> r;
	    if( r != "Y" && r != "y" ) return;
	    resetPlayers();
	}
    }

private:
    void resetPlayers()
    {
	system( "cls" );
	string n;
	for( int p = 0; p < PLAYERS; p++ )
	{
	    _players[p].reset();
	    cout << "Enter name player " << p + 1 << ": "; cin >> n;
	    _players[p].setName( n );
	}

    }

    void praise( int p )
    {
	system( "cls" );
	cout << "CONGRATULATIONS " << _players[p].getName() << ", you are the winner!" << endl << endl;
	cout << "Final Score" << endl;
	drawScoreboard();
	cout << endl << endl;
    }

    void drawScoreboard()
    {
	for( int p = 0; p < PLAYERS; p++ )
	    cout << _players[p].getName() << ": " << _players[p].getCurrScore() << " points" << endl;
	cout << endl;
    }

    bool turn( int p )
    {
	system( "cls" );
	drawScoreboard();
	_players[p].zeroRoundScore();
	string r;
	int die;
	while( true )
	{
	    cout << _players[p].getName() << ", your round score is: " << _players[p].getRoundScore() << endl;
	    cout << "What do you want to do (H)old or (R)oll? "; cin >> r;
	    if( r == "h" || r == "H" )
	    {
		_players[p].addCurrScore();
		return _players[p].getCurrScore() >= MAX_POINTS;
	    }
	    if( r == "r" || r == "R" )
	    {
		die = rand() % 6 + 1;
		if( die == 1 )
		{
	    	    cout << _players[p].getName() << ", your turn is over." << endl << endl;
		    system( "pause" );
		    return false;
		}
		_players[p].addRoundScore( die );
	    }
	    cout << endl;
	}
	return false;
    }

    player	_players[PLAYERS];
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    srand( GetTickCount() );
    pigGame pg;
    pg.play();
    return 0;
}
//--------------------------------------------------------------------------------------------------
