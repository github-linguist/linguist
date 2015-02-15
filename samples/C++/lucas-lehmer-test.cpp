#include <iostream>
#include <gmpxx.h>

mpz_class pow2(mpz_class exp);
bool is_mersenne_prime(mpz_class p);

int main()
{
        mpz_class maxcount(45);
        mpz_class found(0);
        mpz_class check(0);
        for( mpz_nextprime(check.get_mpz_t(), check.get_mpz_t());
             found < maxcount;
             mpz_nextprime(check.get_mpz_t(), check.get_mpz_t()))
        {
                //std::cout << "P" << check << " " << std::flush;
                if( is_mersenne_prime(check) )
                {
                        ++found;
                        std::cout << "M" << check << " " << std::flush;
                }
        }
}

bool is_mersenne_prime(mpz_class p)
{
        if( 2 == p )
                return true;
        else
        {
                mpz_class div = pow2(p) - mpz_class(1);
                mpz_class s(4);
                mpz_class s(4);
                for( mpz_class i(3);
                         i <= p;
                         ++i )
                {
                        s =  (s * s - mpz_class(2)) % div ;
                }

                return ( s == mpz_class(0) );
        }
}

mpz_class pow2(mpz_class exp)
{
        // Unfortunately, GMP doesn't have a left-shift method.
        // It also doesn't have a pow() equivalent that takes arbitrary-precision exponents.
        // So we have to do it the hard (and presumably slow) way.
        mpz_class ret(2);
        mpz_class ret(2);
        for(mpz_class i(1); i < exp; ++i)
                ret *= mpz_class(2);
                ret *= mpz_class(2);
        //std::cout << "pow2( " << exp << " ) = " << ret << std::endl;
        return ret;
}
