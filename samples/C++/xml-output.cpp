#include <vector>
#include <utility>
#include <iostream>
#include <boost/algorithm/string.hpp>

std::string create_xml( std::vector<std::string> & ,std::vector<std::string> & ) ;

int main( ) {
   std::vector<std::string> names , remarks ;
   names.push_back( "April" ) ;
   names.push_back( "Tam O'Shantor" ) ;
   names.push_back ( "Emily" ) ;
   remarks.push_back( "Bubbly, I'm > Tam and <= Emily" ) ;
   remarks.push_back( "Burns: \"When chapman billies leave the street ...\"" ) ;
   remarks.push_back( "Short & shrift" ) ;
   std::cout << "This is in XML:\n" ;
   std::cout << create_xml( names , remarks ) << std::endl ;
   return 0 ;
}

std::string create_xml( std::vector<std::string> & names ,
      std::vector<std::string> & remarks ) {
   std::vector<std::pair<std::string , std::string> > entities ;
   entities.push_back( std::make_pair( "&" , "&amp;" ) ) ;
   entities.push_back( std::make_pair( "<" , "&lt;" ) ) ;
   entities.push_back( std::make_pair( ">" , "&gt;" ) ) ;
   std::string xmlstring ( "<CharacterRemarks>\n" ) ;
   std::vector<std::string>::iterator vsi = names.begin( ) ;
   typedef std::vector<std::pair<std::string , std::string> >::iterator Vpss ;
   for ( ; vsi != names.end( ) ; vsi++ ) {
      for ( Vpss vs = entities.begin( ) ; vs != entities.end( ) ; vs++ ) {
	 boost::replace_all ( *vsi , vs->first , vs->second ) ;
      }
   }
   for ( vsi = remarks.begin( ) ; vsi != remarks.end( ) ; vsi++ ) {
      for ( Vpss vs = entities.begin( ) ; vs != entities.end( ) ; vs++ ) {
	 boost::replace_all ( *vsi , vs->first , vs->second ) ;
      }
   }
   for ( int i = 0 ; i < names.size( ) ; i++ ) {
      xmlstring.append( "\t<Character name=\"").append( names[ i ] ).append( "\">")
	 .append( remarks[ i ] ).append( "</Character>\n" ) ;
   }
   xmlstring.append( "</CharacterRemarks>" ) ;
   return xmlstring ;
}
