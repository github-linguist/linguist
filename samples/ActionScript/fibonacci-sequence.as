public function fib(n:uint):uint
{
    if (n < 2)
        return n;

    return fib(n - 1) + fib(n - 2);
}
