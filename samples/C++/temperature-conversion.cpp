#include <iostream>
#include <iomanip>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
class converter
{
public:
    converter() : KTC( 273.15f ), KTDel( 3.0f / 2.0f ), KTF( 9.0f / 5.0f ), KTNew( 33.0f / 100.0f ),
		  KTRank( 9.0f / 5.0f ), KTRe( 4.0f / 5.0f ), KTRom( 21.0f / 40.0f ) {}

    void convert( float kelvin )
    {
	float cel = kelvin - KTC,
	      del = ( 373.15f - kelvin ) * KTDel,
	      fah = kelvin * KTF - 459.67f,
	      net = cel * KTNew,
	      rnk = kelvin * KTRank,
	      rea = cel * KTRe,
	      rom = cel * KTRom + 7.5f;

	cout << endl << left
	     << "TEMPERATURES:" << endl
	     << "===============" << endl << setw( 13 )
	     << "CELSIUS:" << cel << endl << setw( 13 )
	     << "DELISLE:" << del << endl << setw( 13 )
	     << "FAHRENHEIT:" << fah << endl << setw( 13 )
	     << "KELVIN:" << kelvin << endl  << setw( 13 )
	     << "NEWTON:" << net << endl << setw( 13 )
	     << "RANKINE:" << rnk << endl << setw( 13 )
	     << "REAUMUR:" << rea << endl << setw( 13 )
	     << "ROMER:" << rom << endl << endl << endl;
	}
private:
    const float KTRank, KTC, KTF, KTRe, KTDel, KTNew, KTRom;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    converter con;
    float k;
    while( true )
    {
	cout << "Enter the temperature in Kelvin to convert: ";
	cin >> k;
	con.convert( k );
	system( "pause" );
	system( "cls" );
    }
    return 0;
}
//--------------------------------------------------------------------------------------------------
