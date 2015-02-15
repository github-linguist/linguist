#include <vector>
#include <memory>	// for auto_ptr
#include <cmath>	// for the ceil and log10 and floor functions
#include <iostream>
#include <iomanip>	// for the setw function

using namespace std;

typedef vector< int > IntRow;
typedef vector< IntRow > IntTable;

auto_ptr< IntTable > getSpiralArray( int dimension )
{
	auto_ptr< IntTable > spiralArrayPtr( new IntTable(
		dimension, IntRow( dimension ) ) );

	int numConcentricSquares = static_cast< int >( ceil(
		static_cast< double >( dimension ) / 2.0 ) );

	int j;
	int sideLen = dimension;
	int currNum = 0;

	for ( int i = 0; i < numConcentricSquares; i++ )
	{
		// do top side
		for ( j = 0; j < sideLen; j++ )
			( *spiralArrayPtr )[ i ][ i + j ] = currNum++;

		// do right side
		for ( j = 1; j < sideLen; j++ )
			( *spiralArrayPtr )[ i + j ][ dimension - 1 - i ] = currNum++;

		// do bottom side
		for ( j = sideLen - 2; j > -1; j-- )
			( *spiralArrayPtr )[ dimension - 1 - i ][ i + j ] = currNum++;

		// do left side
		for ( j = sideLen - 2; j > 0; j-- )
			( *spiralArrayPtr )[ i + j ][ i ] = currNum++;

		sideLen -= 2;
	}

	return spiralArrayPtr;
}

void printSpiralArray( const auto_ptr< IntTable >& spiralArrayPtr )
{
	size_t dimension = spiralArrayPtr->size();

	int fieldWidth = static_cast< int >( floor( log10(
		static_cast< double >( dimension * dimension - 1 ) ) ) ) + 2;

	size_t col;
	for ( size_t row = 0; row < dimension; row++ )
	{
		for ( col = 0; col < dimension; col++ )
			cout << setw( fieldWidth ) << ( *spiralArrayPtr )[ row ][ col ];
		cout << endl;
	}
}

int main()
{
	printSpiralArray( getSpiralArray( 5 ) );
}
