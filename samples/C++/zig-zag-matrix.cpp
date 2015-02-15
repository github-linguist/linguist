#include <vector>
#include <memory>	// for auto_ptr
#include <cmath>	// for the log10 and floor functions
#include <iostream>
#include <iomanip>	// for the setw function

using namespace std;

typedef vector< int > IntRow;
typedef vector< IntRow > IntTable;

auto_ptr< IntTable > getZigZagArray( int dimension )
{
	auto_ptr< IntTable > zigZagArrayPtr( new IntTable(
		dimension, IntRow( dimension ) ) );

	// fill along diagonal stripes (oriented as "/")
	int lastValue = dimension * dimension - 1;
	int currDiag = 0;
	int loopFrom;
	int loopTo;
	int i;
	int row;
	int col;
	do
	{
		if ( currDiag < dimension ) // if doing the upper-left triangular half
		{
			loopFrom = 0;
			loopTo = currDiag;
		}
		else // doing the bottom-right triangular half
		{
			loopFrom = currDiag - dimension + 1;
			loopTo = dimension - 1;
		}

		for ( i = loopFrom; i <= loopTo; i++ )
		{
			if ( currDiag % 2 == 0 ) // want to fill upwards
			{
				row = loopTo - i + loopFrom;
				col = i;
			}
			else // want to fill downwards
			{
				row = i;
				col = loopTo - i + loopFrom;
			}

			( *zigZagArrayPtr )[ row ][ col ] = currNum++;
		}

		currDiag++;
	}
	while ( currDiag <= lastValue );

	return zigZagArrayPtr;
}

void printZigZagArray( const auto_ptr< IntTable >& zigZagArrayPtr )
{
	size_t dimension = zigZagArrayPtr->size();

	int fieldWidth = static_cast< int >( floor( log10(
		static_cast< double >( dimension * dimension - 1 ) ) ) ) + 2;

	size_t col;
	for ( size_t row = 0; row < dimension; row++ )
	{
		for ( col = 0; col < dimension; col++ )
			cout << setw( fieldWidth ) << ( *zigZagArrayPtr )[ row ][ col ];
		cout << endl;
	}
}

int main()
{
	printZigZagArray( getZigZagArray( 5 ) );
}
