template Fib(size_t N)
{
    static if (N < 2)
        enum Fib = size_t(1);
    else
        enum Fib = Fib!(N - 2) + Fib!(N - 1);
}
