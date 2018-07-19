#include <windows.h>
#include <iostream>
#include <string>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
enum players { Computer, Human, Draw, None };
const int iWin[8][3] = { { 0, 1, 2 }, { 3, 4, 5 }, { 6, 7, 8 }, { 0, 3, 6 }, { 1, 4, 7 }, { 2, 5, 8 }, { 0, 4, 8 }, { 2, 4, 6 } };

//--------------------------------------------------------------------------------------------------
class ttt
{
public:
    ttt() { _p = rand() % 2; reset(); }

    void play()
    {
	int res = Draw;
	while( true )
	{
	    drawGrid();
	    while( true )
	    {
		if( _p ) getHumanMove();
		else getComputerMove();

		drawGrid();

		res = checkVictory();
		if( res != None ) break;

		++_p %= 2;
	    }

	    if( res == Human ) cout << "CONGRATULATIONS HUMAN --- You won!";
	    else if( res == Computer ) cout << "NOT SO MUCH A SURPRISE --- I won!";
	    else cout << "It's a draw!";

	    cout << endl << endl;

	    string r;
	    cout << "Play again( Y / N )? "; cin >> r;
	    if( r != "Y" && r != "y" ) return;

	    ++_p %= 2;
	    reset();

	}
    }

private:
    void reset()
    {
	for( int x = 0; x < 9; x++ )
	    _field[x] = None;
    }

    void drawGrid()
    {
	system( "cls" );
		
        COORD c = { 0, 2 };
	SetConsoleCursorPosition( GetStdHandle( STD_OUTPUT_HANDLE ), c );

	cout << " 1 | 2 | 3 " << endl;
	cout << "---+---+---" << endl;
	cout << " 4 | 5 | 6 " << endl;
	cout << "---+---+---" << endl;
	cout << " 7 | 8 | 9 " << endl << endl << endl;

	int f = 0;
	for( int y = 0; y < 5; y += 2 )
	    for( int x = 1; x < 11; x += 4 )
	    {
		if( _field[f] != None )
		{
		    COORD c = { x, 2 + y };
		    SetConsoleCursorPosition( GetStdHandle( STD_OUTPUT_HANDLE ), c );
		    string o = _field[f] == Computer ? "X" : "O";
		    cout << o;
		}
		f++;
	    }

        c.Y = 9;
	SetConsoleCursorPosition( GetStdHandle( STD_OUTPUT_HANDLE ), c );
    }

    int checkVictory()
    {
	for( int i = 0; i < 8; i++ )
	{
	    if( _field[iWin[i][0]] != None &&
		_field[iWin[i][0]] == _field[iWin[i][1]] && _field[iWin[i][1]] == _field[iWin[i][2]] )
	    {
		return _field[iWin[i][0]];
	    }
	}

	int i = 0;
	for( int f = 0; f < 9; f++ )
	{
	    if( _field[f] != None )
		i++;
	}
	if( i == 9 ) return Draw;

	return None;
    }

    void getHumanMove()
    {
	int m;
	cout << "Enter your move ( 1 - 9 ) ";
	while( true )
	{
	    m = 0;
	    do
	    { cin >> m; }
	    while( m < 1 && m > 9 );

	    if( _field[m - 1] != None )
		cout << "Invalid move. Try again!" << endl;
	    else break;
	}

	_field[m - 1] = Human;
    }

    void getComputerMove()
    {
	int move = 0;

	do{ move = rand() % 9; }
	while( _field[move] != None );

	for( int i = 0; i < 8; i++ )
	{
	    int try1 = iWin[i][0], try2 = iWin[i][1], try3 = iWin[i][2];

	    if( _field[try1] != None && _field[try1] == _field[try2] && _field[try3] == None )
	    {
		move = try3;
		if( _field[try1] == Computer ) break;
	    }

	    if( _field[try1] != None && _field[try1] == _field[try3] && _field[try2] == None )
	    {			
		move = try2;
		if( _field[try1] == Computer ) break;
	    }

	    if( _field[try2] != None && _field[try2] == _field[try3] && _field[try1] == None )
	    {
		move = try1;
		if( _field[try2] == Computer ) break;
	    }
        }
	_field[move] = Computer;
		
    }


int _p;
int _field[9];
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    srand( GetTickCount() );

    ttt tic;
    tic.play();

    return 0;
}
//--------------------------------------------------------------------------------------------------
