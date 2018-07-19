#include <numeric>
#include <algorithm>
#include <cctype>
#include <iostream>
#include <stdexcept>
#include <iterator>
#include <vector>

using namespace std;

const int weights[] = {1,3,1,7,3,9};
const string valid_chars = "BCDFGHJKLMNPQRSTVWXYZ0123456789";

int char_value(char c){ return isalpha(c) ? c -'A' + 10 : c - '0'; }

int sedol_checksum(string const& sedol)
{
    //Check contents
    if(sedol.size() != 6)
        throw length_error("length of sedol string != 6");
    if(sedol.find_first_not_of(valid_chars) != std::string::npos)
        throw invalid_argument("sedol string contains disallowed characters");

    vector<int> char_values;
    transform(sedol.begin(), sedol.end(), back_inserter(char_values), char_value);
    const int weighted_sum = inner_product(char_values.begin(), char_values.end(), weights, 0);
    return  (10 - (weighted_sum % 10)) % 10;
}

int main()
{
    string inputs[] = {
       "710889",
       "B0YBKJ",
       "406566",
       "B0YBLH",
       "228276",
       "B0YBKL",
       "557910",
       "B0YBKR",
       "585284",
       "B0YBKT",
       "B00030"
   };
   const size_t n_inputs = sizeof(inputs) / sizeof(*inputs);

   for(size_t i = 0; i != n_inputs; ++i)
   {
       cout << inputs[i] << sedol_checksum(inputs[i]) << "\n";
   }
   return 0;
}
