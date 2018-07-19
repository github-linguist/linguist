#include <string>
#include <boost/regex.hpp>
#include <boost/asio.hpp>
#include <vector>
#include <utility>
#include <iostream>
#include <sstream>
#include <cstdlib>
#include <algorithm>
#include <iomanip>

struct Sort { //sorting programming languages according to frequency
   bool operator( ) ( const std::pair<std::string,int> & a , const std::pair<std::string,int> & b )
      const {
	 return a.second > b.second ;
      }
} ;

int main( ) {
   try {
      //setting up an io service , with templated subelements for resolver and query
      boost::asio::io_service io_service ;
      boost::asio::ip::tcp::resolver resolver ( io_service ) ;
      boost::asio::ip::tcp::resolver::query query ( "rosettacode.org" , "http" ) ;
      boost::asio::ip::tcp::resolver::iterator endpoint_iterator = resolver.resolve( query ) ;
      boost::asio::ip::tcp::resolver::iterator end ;
      boost::asio::ip::tcp::socket socket( io_service ) ;
      boost::system::error_code error = boost::asio::error::host_not_found ;
      //looking for an endpoint the socket will be able to connect to
      while ( error && endpoint_iterator != end ) {
	 socket.close( ) ;
	 socket.connect( *endpoint_iterator++ , error ) ;
      }
      if ( error )
	 throw boost::system::system_error ( error ) ;
      //we send a request
      boost::asio::streambuf request ;
      std::ostream request_stream( &request ) ;
      request_stream << "GET " << "/mw/index.php?title=Special:Categories&limit=5000" << " HTTP/1.0\r\n" ;
      request_stream << "Host: " << "rosettacode.org" << "\r\n" ;
      request_stream << "Accept: */*\r\n" ;
      request_stream << "Connection: close\r\n\r\n" ;
      //send the request
      boost::asio::write( socket , request ) ;
      //we receive the response analyzing every line and storing the programming language
      boost::asio::streambuf response ;
      std::istream response_stream ( &response ) ;
      boost::asio::read_until( socket , response , "\r\n\r\n" ) ;
      boost::regex e( "<li><a href=\"[^<>]+?\">([a-zA-Z\\+#1-9]+?)</a>\\s?\\((\\d+) members\\)</li>" ) ;
      //using the wrong regex produces incorrect sorting!!
      std::ostringstream line ;
      std::vector<std::pair<std::string , int> > languages ; //holds language and number of examples
      boost::smatch matches ;
      while ( boost::asio::read( socket , response , boost::asio::transfer_at_least( 1 ) , error ) ) {
	 line << &response ;
	 if ( boost::regex_search( line.str( ) , matches , e ) ) {
	    std::string lang( matches[2].first , matches[2].second ) ;
	    int zahl = atoi ( lang.c_str( ) ) ;
	    languages.push_back( std::make_pair( matches[ 1 ] , zahl ) ) ;
	 }
	 line.str( "") ;//we have to erase the string buffer for the next read
      }
      if ( error != boost::asio::error::eof )
	 throw boost::system::system_error( error ) ;
      //we sort the vector entries , see the struct above
      std::sort( languages.begin( ) , languages.end( ) , Sort( ) ) ;
      int n = 1 ;
      for ( std::vector<std::pair<std::string , int> >::const_iterator spi = languages.begin( ) ;
	    spi != languages.end( ) ; ++spi ) {
	 std::cout << std::setw( 3 ) << std::right << n << '.' << std::setw( 4 ) << std::right <<
	 spi->second   << " - " << spi->first << '\n' ;
	 n++ ;
      }
   } catch ( std::exception &ex ) {
      std::cout << "Exception: " << ex.what( ) << '\n' ;
   }
   return 0 ;
}
