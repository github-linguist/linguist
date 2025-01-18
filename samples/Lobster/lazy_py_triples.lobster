/* Inpired by: https://aras-p.info/blog/2018/12/28/Modern-C-Lamentations/

You can't do "lazy evaluation" in most traditional languages, because you either can't abstract
over the end result (lack of lambdas, like in C), or you can't terminate the lazyness (can't
return from lambda, like in C++ and most other languages).

C++ works around this by having chained iterable objects (ranges).
Haskell has lazy lists, but these require a "de-forestation" optimization to be efficient.

And Lobster can do all of this easily and efficiently because.. you can actually return
out of lambdas to enclosing functions!
*/

// A generator that yields values indefinitely (well, it assumes you'll use "take" below
// before you run out of integer bits).
def ints(start, f):
    while true: f(start++)

// Version with an actual end.
def ints(start, end, f):
    for(end - start + 1) i: f(start + i)

// Generic way to only consume n values out of a lazy generator.
def take(n, gen):
    var i = 0
    gen() f:
        // This returns from take (and thus terminates all generators in between), which is not
        // possible in most languages.
        if i++ == n: return
        f()


// Now we can do generic lazy "Pythagorean Triples", Lobster style:
def py_triples(n, f):
    take(n) yield:
        ints(1) z:
            ints(1, z) x:
                ints(x, z) y:
                    if x * x + y * y == z * z:
                        yield():
                            f([ x, y, z ])

// Note that the language guarantees that lambdas get inlined (and so do named single use / small
// functions), so the above code mostly compiles to a bunch of nested loops, so this code should
// actually be efficient!

// Also note this code is self contained, and fully statically typed.

// Print the first 100.
py_triples(100) t:
    print t

/* Outputs:

[3, 4, 5]
[6, 8, 10]
[5, 12, 13]
[9, 12, 15]
[8, 15, 17]
[12, 16, 20]
[7, 24, 25]
[15, 20, 25]
[10, 24, 26]
...
[65, 156, 169]
[119, 120, 169]
[26, 168, 170]

*/
