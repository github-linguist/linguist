// written for clarity not efficiency.

#include <iostream>
using std::cout;
using std::endl;

#include <boost/array.hpp>
#include <boost/circular_buffer.hpp>

class Subtractive_generator {
private:
    static const int param_i = 55;
    static const int param_j = 24;
    static const int initial_load = 219;
    static const int mod = 1e9;
    boost::circular_buffer<int> r;
public:
    Subtractive_generator(int seed);
    int next();
    int operator()(){return next();}
};

Subtractive_generator::Subtractive_generator(int seed)
:r(param_i)
{
    boost::array<int, param_i> s;
    s[0] = seed;
    s[1] = 1;
    for(int n = 2; n < param_i; ++n){
        int t = s[n-2]-s[n-1];
        if (t < 0 ) t+= mod;
        s[n] = t;
    }

    for(int n = 0; n < param_i; ++n){
	int i = (34 * (n+1)) % param_i;
        r.push_back(s[i]);
    }
    for(int n = param_i; n <= initial_load; ++n) next();
}

int Subtractive_generator::next()
{
    int t = r[0]-r[31];
    if (t < 0) t += mod;
    r.push_back(t);
    return r[param_i-1];
}

int main()
{
    Subtractive_generator rg(292929);

    cout << "result = " << rg() << endl;
    cout << "result = " << rg() << endl;
    cout << "result = " << rg() << endl;
    cout << "result = " << rg() << endl;
    cout << "result = " << rg() << endl;
    cout << "result = " << rg() << endl;
    cout << "result = " << rg() << endl;

    return 0;
}
