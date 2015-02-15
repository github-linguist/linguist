#include <cstdio>
#include <direct.h>

int main() {
	remove( "input.txt" );
	remove( "/input.txt" );
	_rmdir( "docs" );
	_rmdir( "/docs" );

	return 0;
}
