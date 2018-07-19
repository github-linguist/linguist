enum Mu<T> { Roll(@fn(Mu<T>) -> T) }
fn unroll<T>(Roll(f): Mu<T>) -> @fn(Mu<T>) -> T { f }

type RecFunc<A, B> = @fn(@fn(A) -> B) -> @fn(A) -> B;

fn fix<A, B>(f: RecFunc<A, B>) -> @fn(A) -> B {
    let g: @fn(Mu<@fn(A) -> B>) -> @fn(A) -> B =
        |x| |a| f(unroll(x)(x))(a);
    g(Roll(g))
}

fn main() {
    let fac: RecFunc<uint, uint> =
        |f| |x| if (x==0) { 1 } else { f(x-1) * x };
    let fib : RecFunc<uint, uint> =
        |f| |x| if (x<2) { 1 } else { f(x-1) + f(x-2) };

    let ns = std::vec::from_fn(20, |i| i);
    println(fmt!("%?", ns.map(|&n| fix(fac)(n))));
    println(fmt!("%?", ns.map(|&n| fix(fib)(n))));
}
