#include <vector>
#include <string>
#include <iostream>
#include <boost/regex.hpp>
#include <algorithm>
#include <iterator>

int main( ) {
   const std::string xmltext(
      "<inventory title=\"OmniCorp Store #45x10^3\">"
	"<section name=\"health\">"
	  "<item upc=\"123456789\" stock=\"12\">"
	    "<name>Invisibility Cream</name>"
	    "<price>14.50</price>"
	    "<description>Makes you invisible</description>"
	  "</item>"
	  "<item upc=\"445322344\" stock=\"18\">"
	    "<name>Levitation Salve</name>"
	    "<price>23.99</price>"
	    "<description>Levitate yourself for up to 3 hours per application</description>"
	  "</item>"
	"</section>"
	"<section name=\"food\">"
	  "<item upc=\"485672034\" stock=\"653\">"
	    "<name>Blork and Freen Instameal</name>"
	    "<price>4.95</price>"
	    "<description>A tasty meal in a tablet; just add water</description>"
	  "</item>"
	  "<item upc=\"132957764\" stock=\"44\">"
	    "<name>Grob winglets</name>"
	    "<price>3.56</price>"
	    "<description>Tender winglets of Grob. Just add water</description>"
	  "</item>"
	"</section>"
      "</inventory>" ) ;
   std::string::size_type found = xmltext.find( "<item" , 0 ) ; //beginning of first item
   std::string::size_type foundnext = xmltext.find(  "</item>" , found + 5 ) ; //and its end
   std::cout << "The first item is\n" << xmltext.substr( found + 5 , foundnext - ( found + 5 ) ) << '\n' ;
   std::string::const_iterator start , end ;
   start = xmltext.begin( ) ;
   end = xmltext.end( ) ;
   boost::match_results<std::string::const_iterator> what ;
   boost::regex pricefind( "<price>(\\d+\\.?\\d+)</price>" ) ;//this regex finds the prices
   start = xmltext.begin( ) ;
   std::cout << "The prices are:\n" ;
   while ( boost::regex_search( start , end , what , pricefind ) ) {
      std::string price( what[ 1 ].first , what[ 1 ].second ) ;//find the first price
      std::cout << price << std::endl ;
      start = what[ 1 ].second ;                               //continue search after first price found
   }
   start = xmltext.begin( ) ;
   std::vector<std::string> names ;
   boost::regex namefind( "<name>(.+?)</name>" ) ;            //find characters, be greedy!
   while ( boost::regex_search ( start , end , what , namefind ) ) {
      std::string name ( what[ 1 ].first , what[ 1 ].second ) ;
      names.push_back( name ) ;
      start = what[ 1 ].second ;
   }
   std::cout << "The following name elements were found in the xml string:\n" ;
   std::copy( names.begin( ) , names.end( ) , std::ostream_iterator<std::string>( std::cout , "\n" )) ;
   return 0 ;
}
