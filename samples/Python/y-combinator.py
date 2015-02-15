>>> Y = lambda f: (lambda x: x(x))(lambda y: f(lambda *args: y(y)(*args)))
>>> fac = lambda f: lambda n: (1 if n<2 else n*f(n-1))
>>> [ Y(fac)(i) for i in range(10) ]
[1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]
>>> fib = lambda f: lambda n: 0 if n == 0 else (1 if n == 1 else f(n-1) + f(n-2))
>>> [ Y(fib)(i) for i in range(10) ]
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
