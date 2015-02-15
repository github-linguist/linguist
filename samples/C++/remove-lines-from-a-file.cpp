#include <fstream>
#include <iostream>
#include <string>
#include <cstdlib>
#include <list>

void deleteLines( const std::string & , int , int ) ;

int main( int argc, char * argv[ ] ) {
   if ( argc != 4 ) {
      std::cerr << "Error! Invoke with <deletelines filename startline skipnumber>!\n" ;
      return 1 ;
   }
   std::string filename( argv[ 1 ] ) ;
   int startfrom = atoi( argv[ 2 ] ) ;
   int howmany = atoi( argv[ 3 ] ) ;
   deleteLines ( filename , startfrom , howmany ) ;
   return 0 ;
}

void deleteLines( const std::string & filename , int start , int skip ) {
   std::ifstream infile( filename.c_str( ) , std::ios::in ) ;
   if ( infile.is_open( ) ) {
      std::string line ;
      std::list<std::string> filelines ;
      while ( infile ) {
	 getline( infile , line ) ;
	 filelines.push_back( line ) ;
      }
      infile.close( ) ;
      if ( start > filelines.size( ) ) {
	 std::cerr << "Error! Starting to delete lines past the end of the file!\n" ;
	 return ;
      }
      if ( ( start + skip ) > filelines.size( ) ) {
	 std::cerr << "Error! Trying to delete lines past the end of the file!\n" ;
	 return ;
      }
      std::list<std::string>::iterator deletebegin = filelines.begin( ) , deleteend ;
      for ( int i = 1 ; i < start ; i++ )
	 deletebegin++ ;
      deleteend = deletebegin ;
      for( int i = 0 ; i < skip ; i++ )
	 deleteend++ ;
      filelines.erase( deletebegin , deleteend ) ;
      std::ofstream outfile( filename.c_str( ) , std::ios::out | std::ios::trunc ) ;
      if ( outfile.is_open( ) ) {
	 for ( std::list<std::string>::const_iterator sli = filelines.begin( ) ;
	       sli != filelines.end( ) ; sli++ )
	    outfile << *sli << "\n" ;
      }
      outfile.close( ) ;
   }
   else {
      std::cerr << "Error! Could not find file " << filename << " !\n" ;
      return ;
   }
}
