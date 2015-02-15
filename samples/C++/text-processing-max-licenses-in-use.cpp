#include <fstream>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>

int main()
{
    const char logfilename[] = "mlijobs.txt";
    std::ifstream logfile(logfilename);

    if (!logfile.is_open())
    {
        std::cerr << "Error opening: " << logfilename << "\n";
        return -1;
    }

    int license = 0, max_license = 0;
    std::vector<std::string> max_timestamp;

    for (std::string logline; std::getline(logfile, logline); )
    {
        std::string action(logline.substr(8,3));

        if (action == "OUT")
        {
            if (++license >= max_license)
            {
                if (license > max_license)
                {
                    max_license = license;
                    max_timestamp.clear();
                }
                max_timestamp.push_back(logline.substr(14, 19));
            }
        }
        else if (action == "IN ")
        {
            --license;
        }
    }

    std::cout << "License count at log end: " << license
        << "\nMaximum simultaneous license: " << max_license
        << "\nMaximum license time(s):\n";

    std::copy(max_timestamp.begin(), max_timestamp.end(),
        std::ostream_iterator<std::string>(std::cout, "\n"));
}
