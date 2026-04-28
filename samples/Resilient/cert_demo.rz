// RES-071 demo: a contract clause that the cheap folder cannot reduce
// (because `x` is a free variable) but that is a universal tautology
// for stock Z3. With `--features z3` and `--emit-certificate <DIR>`,
// the driver will write an SMT-LIB2 file proving the obligation.
fn ident_round(int x) -> int
    requires x + 0 == x
{
    return x;
}

fn main() {
    let r = ident_round(42);
    println(r);
}

main();
