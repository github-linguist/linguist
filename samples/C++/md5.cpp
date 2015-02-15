#include <string>
#include <iostream>
#include "Poco/MD5Engine.h"
#include "Poco/DigestStream.h"

using Poco::DigestEngine ;
using Poco::MD5Engine ;
using Poco::DigestOutputStream ;

int main( ) {
   std::string myphrase ( "The quick brown fox jumped over the lazy dog's back" ) ;
   MD5Engine md5 ;
   DigestOutputStream outstr( md5 ) ;
   outstr << myphrase ;
   outstr.flush( ) ; //to pass everything to the digest engine
   const DigestEngine::Digest& digest = md5.digest( ) ;
   std::cout << myphrase << " as a MD5 digest :\n" << DigestEngine::digestToHex( digest )
      << " !" << std::endl ;
   return 0 ;
}
