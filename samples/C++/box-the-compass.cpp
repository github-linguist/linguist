#include <string>
#include <boost/array.hpp>
#include <boost/assign/list_of.hpp>
#include <boost/format.hpp>
#include <boost/foreach.hpp>
#include <iostream>
#include <math.h>
using std::string;
using namespace boost::assign;

int get_Index(float angle)
{
   return static_cast<int>(floor(angle / 11.25 +0.5 )) % 32 + 1;
}

string get_Abbr_From_Index(int i)
{
    static boost::array<std::string, 32> points(list_of
            ("N")("NbE")("NNE")("NEbN")("NE")("NEbE")("ENE")("EbN")
            ("E")("EbS")("ESE")("SEbE")("SE")("SEbS")("SSE")("SbE")
            ("S")("SbW")("SSW")("SWbS")("SW")("SWbW")("WSW")("WbS")
            ("W")("WbN")("WNW")("NWbW")("NW")("NWbN")("NNW")("NbW"));
    return points[i-1];
}

string Build_Name_From_Abbreviation(string a)
{
    string retval;
    for (int i = 0; i < a.size(); ++i){
        if ((1 == i) && (a[i] != 'b') && (a.size() == 3)) retval += "-";
        switch (a[i]){
            case 'N' : retval += "north"; break;
            case 'S' : retval += "south"; break;
            case 'E' : retval += "east";  break;
            case 'W' : retval += "west";  break;
            case 'b' : retval += " by ";  break;
        }
    }
    retval[0] = toupper(retval[0]);
    return retval;
}

int main()
{
    boost::array<float,33> headings(list_of
            (0.0)(16.87)(16.88)(33.75)(50.62)(50.63)(67.5)(84.37)(84.38)(101.25)
            (118.12)(118.13)(135.0)(151.87)(151.88)(168.75)(185.62)(185.63)(202.5)
            (219.37)(219.38)(236.25)(253.12)(253.13)(270.0)(286.87)(286.88)(303.75)
            (320.62)(320.63)(337.5)(354.37)(354.38));
    int i;
    boost::format f("%1$4d %2$-20s %3$_7.2f");

    BOOST_FOREACH(float a, headings)
    {
        i = get_Index(a);
        std::cout << f % i %  Build_Name_From_Abbreviation(get_Abbr_From_Index(i)) % a << std::endl;
    }
    return 0;
}
