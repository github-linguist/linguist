#include <iostream>
using namespace std;

int toInt(const char c)
{
    return c-'0';
}

int confirm( const char *id)
{
    bool is_odd_dgt = true;
    int s = 0;
    const char *cp;

    for(cp=id; *cp; cp++);
    while(cp > id) {
        --cp;
        int k = toInt(*cp);
        if (is_odd_dgt) {
            s += k;
        }
        else {
            s += (k!=9)? (2*k)%9 : 9;
        }
	is_odd_dgt = !is_odd_dgt;
    }
    return 0 == s%10;
}

int main( )
{
    const char * t_cases[] = {
        "49927398716",
        "49927398717",
        "1234567812345678",
        "1234567812345670",
        NULL,
    };
    for ( const char **cp = t_cases; *cp; cp++) {
        cout << *cp << ": " << confirm(*cp) << endl;
    }
    return 0;
}
