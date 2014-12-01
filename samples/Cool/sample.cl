(* Refer to Alex Aiken, "The Cool Reference Manual":
    http://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf
   for language specification.
*)

-- Exhibit various language constructs
class Sample {
    testCondition(x: Int): Bool {
        if x = 0
        then false
        else
            if x < (1 + 2) * 3
            then true
            else false
            fi
        fi
    };

    testLoop(y: Int): Bool {
        while y > 0 loop
        {
            if not condition(y)
            then y <- y / 2
            else y <- y - 1;
        }
        pool
    };

    testAssign(z: Int): Bool {
        i : Int;
        i <- ~z;
    };

    testCase(var: Sample): SELF_TYPE {
        io : IO <- new IO;
        case var of
            a : A => io.out_string("Class type is A\n");
            b : B => io.out_string("Class type is B\n");
            s : Sample => io.out_string("Class type is Sample\n");
            o : Object => io.out_string("Class type is object\n");
        esac
    };

    testLet(i: Int): Int {
        let (a: Int in
            let(b: Int <- 3, c: Int <- 4 in
                {
                    a <- 2;
                    a * b * 2 / c;
                }
            )
        )
    };
};

-- Used to test subclasses
class A inherits Sample {};
class B inherits A {};

class C {
    main() : Int {
        (new Sample).testLet(1)
    };
};

-- "Hello, world" example
class Main inherits IO {
    main(): SELF_TYPE {
        out_string("Hello, World.\n")
    };
};
