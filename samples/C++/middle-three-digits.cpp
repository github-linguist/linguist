#include <iostream>
#include <sstream>
#include <string>

// @author Martin Ettl
// @date   2013-02-04

/**
 * Convert variables of type T to std::string
 *
 *
 * @param d --> digit of type T
 *
 * @return <-- the corresponding string value
 */
template <typename T> const std::string toString(const T &d)
{
    std::ostringstream result;
    result << d;
    return result.str();
}

/**
 * Determine the middle n digits of the integer. If it is not possible to determine the
 * the middle n digits, an empty string is provided.
 *
 * @param iDigit --> The digit to test
 * @param n      --> The number of digits inbetween
 *
 * @return <-- the middle three digits
 */
std::string strMiddleNDigits(int iDigit, const int &n)
{
    // is negative: --> convert to a positive number
    if(iDigit<0)
    {
        iDigit*=-1;
    }
    // convert to string
    std::string strNumber (toString(iDigit));
    size_t len(strNumber.length());
    if( (len < n) || (len % 2 == 0) )
    {
        return "";
    }
    size_t mid(len/2);
    return strNumber.substr(mid-n/2, n);
}

/**
 * Determine the middle three digits of the integer. If it is not possible to determine the
 * the middle three digits, an empty string is provided.
 *
 * @param iDigit --> The digit to test
 *
 * @return <-- the middle three digits
 */
std::string strMiddleThreeDigits(int iDigit)
{
    return strMiddleNDigits(iDigit,3);
}

int main()
{
    const int iPassing[] = {123, 12345, 1234567, 987654321, 10001, -10001,
                            -123, -100, 100, -12345
                           };
    for(unsigned int ui = 0; ui < 10; ++ui)
    {
        std::cout << "strMiddleThreeDigits("<< iPassing[ui] <<"): "
                  << strMiddleThreeDigits(iPassing[ui])<< "\n";
    }

    const int iFailing[] = {1, 2, -1, -10, 2002, -2002, 0};
    for(unsigned int ui = 0; ui < 7; ++ui)
    {
        std::string strResult = strMiddleThreeDigits(iFailing[ui]);
        std::cout << "strMiddleThreeDigits("<< iFailing[ui] <<"): "
                  << (strResult.empty()?"Need odd and >= 3 digits":strResult)
                  << "\n";
    }

    return 0;
}
