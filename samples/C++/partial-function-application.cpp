#include <utility> // For declval.
#include <algorithm>
#include <array>
#include <iterator>
#include <iostream>

/* Partial application helper. */
template< class F, class Arg >
struct PApply
{
    F f;
    Arg arg;

    tempalte< class F_, class Arg_ >
    PApply( F_&& f, Arg_&& arg )
        : f(std::forward<F_>(f)), arg(std::forward<Arg_>(arg))
    {
    }

    /*
     * The return type of F only gets deduced based on the number of arguments
     * supplied. PApply otherwise has no idea whether f takes 1 or 10 args.
     */
    template< class ... Args >
    auto operator() ( Args&& ...args )
        -> decltype( f(arg,std::declval<Args>()...) )
    {
        return f( arg, std::forward<Args>(args)... );
    }
};

template< class F, class Arg >
PApply<F,Arg> papply( F&& f, Arg&& arg )
{
    return PApply<F,Arg>( std::forward<F>(f), std::forward<Arg>(arg) );
}

/* Apply f to cont. */
template< class F >
std::array<int,4> fs( F&& f, std::array<int,4> cont )
{
    std::transform( std::begin(cont), std::end(cont), std::begin(cont),
                    std::forward<F>(f) );
    return cont;
}

std::ostream& operator << ( std::ostream& out, const std::array<int,4>& c )
{
    std::copy( std::begin(c), std::end(c),
               std::ostream_iterator<int>(out, ", ") );
    return out;
}

int f1( int x ) { return x * 2; }
int f2( int x ) { return x * x; }

int main()
{
    std::array<int,4> xs = {{ 0, 1, 2, 3 }};
    std::array<int,4> ys = {{ 2, 4, 6, 8 }};

    auto fsf1 = papply( fs<decltype(f1)>, f1 );
    auto fsf2 = papply( fs<decltype(f2)>, f2 );

    std::cout << "xs:\n"
              << "\tfsf1: " << fsf1(xs) << '\n'
              << "\tfsf2: " << fsf2(xs) << "\n\n"
              << "ys:\n"
              << "\tfsf1: " << fsf1(ys) << '\n'
              << "\tfsf2: " << fsf2(ys) << '\n';
}
