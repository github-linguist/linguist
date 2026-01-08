#import "Basic";

do_some_work :: (a: int, b: int) -> int {
    #asm {
        add a, b;
    }
    return a;
}

A :: #run do_some_work(10, 13);

main :: () {
    print("A: %\n", A); // => A: 23 
}