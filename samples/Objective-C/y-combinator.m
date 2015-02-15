#import <Foundation/Foundation.h>

typedef int (^Func)(int);
typedef Func (^FuncFunc)(Func);
typedef Func (^RecursiveFunc)(id); // hide recursive typing behind dynamic typing

Func fix (FuncFunc f) {
  RecursiveFunc r =
    ^(id y) {
      RecursiveFunc w = y; // cast value back into desired type
      return f(^(int x) {
        return w(w)(x);
      });
    };
  return r(r);
}

int main (int argc, const char *argv[]) {
  @autoreleasepool {

    FuncFunc almost_fac = ^Func(Func f) {
      return ^(int n) {
        if (n <= 1) return 1;
        return n * f(n - 1);
      };
    };

    FuncFunc almost_fib = ^Func(Func f) {
      return ^(int n) {
        if (n <= 2) return 1;
        return  f(n - 1) + f(n - 2);
      };
    };

    Func fib = fix(almost_fib);
    Func fac = fix(almost_fac);
    NSLog(@"fib(10) = %d", fib(10));
    NSLog(@"fac(10) = %d", fac(10));

  }
  return 0;
}
