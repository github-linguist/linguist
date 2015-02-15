template<int N>
struct Half
{
        enum { Result = N >> 1 };
};

template<int N>
struct Double
{
        enum { Result = N << 1 };
};

template<int N>
struct IsEven
{
        static const bool Result = (N & 1) == 0;
};

template<int Multiplier, int Multiplicand>
struct EthiopianMultiplication
{
        template<bool Cond, int Plier, int RunningTotal>
        struct AddIfNot
        {
                enum { Result = Plier + RunningTotal };
        };
        template<int Plier, int RunningTotal>
        struct AddIfNot <true, Plier, RunningTotal>
        {
                enum { Result = RunningTotal };
        };

        template<int Plier, int Plicand, int RunningTotal>
        struct Loop
        {
                enum { Result = Loop<Half<Plier>::Result, Double<Plicand>::Result,
                       AddIfNot<IsEven<Plier>::Result, Plicand, RunningTotal >::Result >::Result };
        };
        template<int Plicand, int RunningTotal>
        struct Loop <0, Plicand, RunningTotal>
        {
                enum { Result = RunningTotal };
        };

        enum { Result = Loop<Multiplier, Multiplicand, 0>::Result };
};

#include <iostream>

int main(int, char **)
{
        std::cout << EthiopianMultiplication<17, 54>::Result << std::endl;
        return 0;
}
