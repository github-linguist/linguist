#include <iostream>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
class mRND
{
public:
    void seed( unsigned int s ) { _seed = s; }

protected:
    mRND() : _seed( 0 ), _a( 0 ), _c( 0 ), _m( 2147483648 ) {}
    int rnd() { return( _seed = ( _a * _seed + _c ) % _m ); }

    int _a, _c;
    unsigned int _m, _seed;
};
//--------------------------------------------------------------------------------------------------
class MS_RND : public mRND
{
public:
    MS_RND()  { _a = 214013; _c = 2531011; }
    int rnd() { return mRND::rnd() >> 16; }
};
//--------------------------------------------------------------------------------------------------
class BSD_RND : public mRND
{
public:
    BSD_RND() { _a = 1103515245; _c = 12345; }
    int rnd() { return mRND::rnd(); }
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    BSD_RND bsd_rnd;
    MS_RND ms_rnd;

    cout << "MS RAND:" << endl << "========" << endl;
    for( int x = 0; x < 10; x++ )
	cout << ms_rnd.rnd() << endl;

    cout << endl  << "BSD RAND:" << endl << "=========" << endl;
    for( int x = 0; x < 10; x++ )
	cout << bsd_rnd.rnd() << endl;

    cout << endl << endl;
    system( "pause" );
    return 0;
}
//--------------------------------------------------------------------------------------------------
