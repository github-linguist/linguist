// ============================================================================
// Pattern Matching Test Suite
// ============================================================================

describe("Pattern Matching", fn() {
    test("match with literal patterns", fn() {
        fn describe(x) {
            return match x {
                0 => "zero",
                1 => "one",
                _ => "other"
            };
        }
        assert_eq(describe(0), "zero");
        assert_eq(describe(1), "one");
        assert_eq(describe(99), "other");
    });

    test("match with variable binding", fn() {
        fn describe(x) {
            return match x {
                0 => "zero",
                n => "number: " + str(n)
            };
        }
        assert_eq(describe(0), "zero");
        assert_eq(describe(42), "number: 42");
    });

    test("match with guards", fn() {
        fn classify(x) {
            return match x {
                n if n < 0 => "negative",
                n if n == 0 => "zero",
                n if n > 0 => "positive"
            };
        }
        assert_eq(classify(-5), "negative");
        assert_eq(classify(0), "zero");
        assert_eq(classify(5), "positive");
    });

    test("match with array patterns", fn() {
        fn first(arr) {
            return match arr {
                [] => "empty",
                [x] => "single: " + str(x),
                [x, y] => "pair: " + str(x) + ", " + str(y),
                _ => "many"
            };
        }
        assert_eq(first([]), "empty");
        assert_eq(first([1]), "single: 1");
        assert_eq(first([1, 2]), "pair: 1, 2");
        assert_eq(first([1, 2, 3]), "many");
    });

    test("match with hash patterns", fn() {
        fn get_name(person) {
            return match person {
                {name: n, age: a} => n + " is " + str(a),
                {name: n} => n,
                _ => "unknown"
            };
        }
        assert_eq(get_name({name: "Alice", age: 30}), "Alice is 30");
        assert_eq(get_name({name: "Bob"}), "Bob");
        assert_eq(get_name({foo: "bar"}), "unknown");
    });

    test("nested match", fn() {
        fn evaluate(x) {
            if (x < 0) {
                if (x == -1) {
                    return "negative one";
                }
                return "negative";
            }
            if (x == 0) {
                return "zero";
            }
            return "positive: " + str(x);
        }
        assert_eq(evaluate(-1), "negative one");
        assert_eq(evaluate(-5), "negative");
        assert_eq(evaluate(0), "zero");
        assert_eq(evaluate(5), "positive: 5");
    });

    test("match with multiple guards", fn() {
        fn fizzbuzz(n) {
            return match n {
                n if n % 15 == 0 => "fizzbuzz",
                n if n % 5 == 0 => "buzz",
                n if n % 3 == 0 => "fizz",
                n => str(n)
            };
        }
        assert_eq(fizzbuzz(3), "fizz");
        assert_eq(fizzbuzz(5), "buzz");
        assert_eq(fizzbuzz(15), "fizzbuzz");
        assert_eq(fizzbuzz(7), "7");
    });
});
