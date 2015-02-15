#include <iostream>
#include <functional>

template <typename F>
struct RecursiveFunc {
	std::function<F(RecursiveFunc)> o;
};

template <typename A, typename B>
std::function<B(A)> fix (std::function<std::function<B(A)>(std::function<B(A)>)> f) {
	RecursiveFunc<std::function<B(A)>> r = {
		std::function<std::function<B(A)>(RecursiveFunc<std::function<B(A)>>)>([f](RecursiveFunc<std::function<B(A)>> w) {
			return f(std::function<B(A)>([w](A x) {
				return w.o(w)(x);
			}));
		})
	};
	return r.o(r);
}

typedef std::function<int(int)> Func;
typedef std::function<Func(Func)> FuncFunc;
FuncFunc almost_fac = [](Func f) {
	return Func([f](int n) {
		if (n <= 1) return 1;
		return n * f(n - 1);
	});
};

FuncFunc almost_fib = [](Func f) {
	return Func([f](int n) {
	 	if (n <= 2) return 1;
		return  f(n - 1) + f(n - 2);
	});
};

int main() {
	auto fib = fix(almost_fib);
	auto fac = fix(almost_fac);
	std::cout << "fib(10) = " << fib(10) << std::endl;
	std::cout << "fac(10) = " << fac(10) << std::endl;
	return 0;
}
