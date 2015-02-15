// Display the current date in the formats of "2007-11-10"
// and "Sunday, November 10, 2007".

#include <vector>
#include <string>
#include <iostream>
#include <ctime>

/** Return the current date in a string, formatted as either ISO-8601
 *  or "Weekday-name, Month-name Day, Year".
 *
 *  The date is initialized when the object is created and will return
 *  the same date for the lifetime of the object.  The date returned
 *  is the date in the local timezone.
 */
class Date
{
    struct tm ltime;

public:
    /// Default constructor.
    Date()
    {
        time_t t = time(0);
        localtime_r(&t, &ltime);
    }

    /** Return the date based on a format string.  The format string is
     *  fed directly into strftime().  See the strftime() documentation
     *  for information on the proper construction of format strings.
     *
     *  @param[in] fmt is a valid strftime() format string.
     *
     *  @return a string containing the formatted date, or a blank string
     *      if the format string was invalid or resulted in a string that
     *      exceeded the internal buffer length.
     */
    std::string getDate(const char* fmt)
    {
        char out[200];
        size_t result = strftime(out, sizeof out, fmt, &ltime);
        return std::string(out, out + result);
    }

    /** Return the date in ISO-8601 date format.
     *
     *  @return a string containing the date in ISO-8601 date format.
     */
    std::string getISODate() {return getDate("%F");}

    /** Return the date formatted as "Weekday-name, Month-name Day, Year".
     *
     *  @return a string containing the date in the specified format.
     */
    std::string getTextDate() {return getDate("%A, %B %d, %Y");}
};

int main()
{
    Date d;
    std::cout << d.getISODate() << std::endl;
    std::cout << d.getTextDate() << std::endl;
    return 0;
}
