#include <iostream>
#include <string>
#include <boost/asio.hpp>
#include <boost/regex.hpp>
int main()
{
    boost::asio::ip::tcp::iostream s("tycho.usno.navy.mil", "http");
    if(!s)
        std::cout << "Could not connect to tycho.usno.navy.mil\n";
    s  << "GET /cgi-bin/timer.pl HTTP/1.0\r\n"
       << "Host: tycho.usno.navy.mil\r\n"
       << "Accept: */*\r\n"
       << "Connection: close\r\n\r\n" ;
    for(std::string line; getline(s, line); )
    {
        boost::smatch matches;
        if(regex_search(line, matches, boost::regex("<BR>(.+\\s+UTC)") ) )
        {
            std::cout << matches[1] << '\n';
            break;
        }
    }
}
