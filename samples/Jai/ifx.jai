#import "Basic";

Thing :: struct {
    name:  string;
    value: int;
}

get_default_name :: () -> string { return "Alice"; }

factorial :: (x: int) -> int {
    return ifx x <= 1 then 1 else x*factorial(x-1);   // (1)
}

is_even :: (value: int) -> bool {
    return !cast(bool)(value & 1);
}

main :: () {
    a := 0;
    b := 100;
    c := ifx a > b 10 else 1000;  // (2)    
    print("c is %\n", c);         // => c is 1000
    
    thing := *Thing.{"Liz", 42};
    // name: string;
    // if thing {
    //     name = thing.name;
    // } else {
    //     name = get_default_name();
    // }
    
    // one-liner with ifx:
    name := ifx thing then thing.name else get_default_name(); // (3)

    // with code blocks:
    // name := ifx thing {                                     // (4)
    //     print("This is the true block.\n");
    //     factorial(5);
    //     thing.name;
    // } else {
    //     print("We are about to get the default name.\n");
    //     x := 3;
    //     print("Really, it is going to happen.\n");
    //     get_default_name();
    // }
    // => This is the true block.
    print("Your name is %\n", name); // => Your name is Liz

    x := 7;
    y := ifx x then x else 1;
    // can be shortened to:
    y2 := ifx x else 1;     // (5)
    print("y2 is %\n", y2); // => y2 is 7
    y3 := ifx x > 5 else 0; // (6)
    print("y3 is %\n", y3); // => y3 is 7
    y4 := ifx is_even(x);
    print("y4 is %\n", y4); // => y4 is 0
    y5 := ifx !is_even(x);
    print("y5 is %\n", y5); // => y4 is 7
}