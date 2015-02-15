#include <functional>
#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>

using std::cout;
using std::endl;
using std::vector;
using std::function;
using std::transform;
using std::back_inserter;

typedef function<double(double)> FunType;

vector<FunType> A = {sin, cos, tan, [](double x) { return x*x*x; } };
vector<FunType> B = {asin, acos, atan, [](double x) { return exp(log(x)/3); } };

template <typename A, typename B, typename C>
function<C(A)> compose(function<C(B)> f, function<B(A)> g) {
    return [f,g](A x) { return f(g(x)); };
}

int main() {
    vector<FunType> composedFuns;
    auto exNums = {0.0, 0.2, 0.4, 0.6, 0.8, 1.0};

    transform(B.begin(), B.end(),
                A.begin(),
                back_inserter(composedFuns),
                compose<double, double, double>);

    for (auto num: exNums)
        for (auto fun: composedFuns)
            cout << u8"f\u207B\u00B9.f(" << num << ") = " << fun(num) << endl;

    return 0;
}
