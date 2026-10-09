module fib;

int fibonacci(int n)
{
    if (n == 0)
        return 0;
    if (n == 1)
        return 1;

    int a = 0, b = 1, fib = 0;
    for (int i = 0; i < n ; i++)
    {
        fib = a + b;

        a = b;
        b = fib;
    }
    return fib;
}