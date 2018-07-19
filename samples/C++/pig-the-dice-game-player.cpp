#include <windows.h>
#include <iostream>
#include <string>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const int PLAYERS = 4, MAX_POINTS = 100;

//--------------------------------------------------------------------------------------------------
enum Moves { ROLL, HOLD };

//--------------------------------------------------------------------------------------------------
class player
{
public:
    player()                     { current_score = round_score = 0; }
    void addCurrScore()          { current_score += round_score; }
    int getCurrScore()           { return current_score; }
    int getRoundScore()          { return round_score; }
    void addRoundScore( int rs ) { round_score += rs; }
    void zeroRoundScore()        { round_score = 0; }
    virtual int getMove() = 0;
    virtual ~player() {}

protected:
    int current_score, round_score;
};
//--------------------------------------------------------------------------------------------------
class RAND_Player : public player
{
    virtual int getMove()
    {
	if( round_score + current_score >= MAX_POINTS ) return HOLD;

	if( rand() % 10 < 5 ) return ROLL;
	if( round_score > 0 ) return HOLD;
	return ROLL;
    }
};
//--------------------------------------------------------------------------------------------------
class Q2WIN_Player : public player
{
    virtual int getMove()
    {
	if( round_score + current_score >= MAX_POINTS ) return HOLD;
		
	int q = MAX_POINTS - current_score;
	if( q < 6 ) return ROLL;
	q /= 4;
	if( round_score < q ) return ROLL;
	return HOLD;
    }
};
//--------------------------------------------------------------------------------------------------
class AL20_Player : public player
{
    virtual int getMove()
    {
	if( round_score + current_score >= MAX_POINTS ) return HOLD;

	if( round_score < 20 ) return ROLL;
	return HOLD;
    }
};
//--------------------------------------------------------------------------------------------------
class AL20T_Player : public player
{
    virtual int getMove()
    {
	if( round_score + current_score >= MAX_POINTS ) return HOLD;

	int d = ( 100 * round_score ) / 20;
	if( round_score < 20 && d < rand() % 100 ) return ROLL;
	return HOLD;
    }
};
//--------------------------------------------------------------------------------------------------
class Auto_pigGame
{
public:
    Auto_pigGame()
    {
	_players[0] = new RAND_Player();
	_players[1] = new Q2WIN_Player();
	_players[2] = new AL20_Player();
        _players[3] = new AL20T_Player();
    }

    ~Auto_pigGame()
    {
	delete _players[0];
	delete _players[1];
	delete _players[2];
        delete _players[3];
    }

    void play()
    {
	int die, p = 0;
	bool endGame = false;

	while( !endGame )
	{
	    switch( _players[p]->getMove() )
	    {
		case ROLL:
	    	    die = rand() % 6 + 1;
		    if( die == 1 )
		    {
			cout << "Player " << p + 1 << " rolled " << die << " - current score: " << _players[p]->getCurrScore() << endl << endl;
			nextTurn( p );
			continue;
		    }
		    _players[p]->addRoundScore( die );
		    cout << "Player " << p + 1 << " rolled " << die << " - round score: " << _players[p]->getRoundScore() << endl;
		break;
		case HOLD:
	    	    _players[p]->addCurrScore();
		    cout << "Player " << p + 1 << " holds - current score: " << _players[p]->getCurrScore() << endl << endl;
		    if( _players[p]->getCurrScore() >= MAX_POINTS )
			endGame = true;
		    else nextTurn( p );

	    }
	}
	showScore();
    }

private:
    void nextTurn( int& p )
    {
	_players[p]->zeroRoundScore();
	++p %= PLAYERS;
    }

    void showScore()
    {
	cout << endl;
	cout << "Player   I (RAND): "  << _players[0]->getCurrScore() << endl;
	cout << "Player  II (Q2WIN): " << _players[1]->getCurrScore() << endl;
        cout << "Player III (AL20): " << _players[2]->getCurrScore() << endl;
	cout << "Player  IV (AL20T): "  << _players[3]->getCurrScore() << endl << endl << endl;

	system( "pause" );
    }

    player*	_players[PLAYERS];
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    srand( GetTickCount() );
    Auto_pigGame pg;
    pg.play();
    return 0;
}
//--------------------------------------------------------------------------------------------------
