#include <iostream>
#include <string>
#include <list>
#include <algorithm>
#include <iterator>

using namespace std;

template<typename OutIt>
void sierpinski(int n, OutIt result)
{
    if( n == 0 )
    {
        *result++ = "*";
    }
    else
    {
        list<string> prev;
        sierpinski(n-1, back_inserter(prev));

        string sp(1 << (n-1), ' ');
        result = transform(prev.begin(), prev.end(),
            result,
            [sp](const string& x) { return sp + x + sp; });
        transform(prev.begin(), prev.end(),
            result,
            [sp](const string& x) { return x + " " + x; });
    }
}

int main()
{
    sierpinski(4, ostream_iterator<string>(cout, "\n"));
    return 0;
}
