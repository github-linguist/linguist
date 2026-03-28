// Krypton language features — string operations, control flow, functions
// Source: https://github.com/krypton-lang/krypton/blob/main/examples/features_test.k
// License: MIT

func add(a, b) {
    emit a + b
}

func greet(name) {
    emit "Hello, " + name
}

func fibonacci(n) {
    if n <= 1 {
        emit n
    }
    let a = 0
    let b = 1
    let i = 2
    while i <= n {
        let temp = a + b
        a = b
        b = temp
        i = i + 1
    }
    emit b
}

func countChars(text, target) {
    let i = 0
    let count = 0
    while i < len(text) {
        if text[i] == target {
            count = count + 1
        }
        i = i + 1
    }
    emit count
}

just run {
    // Arithmetic and function calls
    let x = add(3, 4)
    print(x)

    // String concatenation via function
    let msg = greet("Krypton")
    print(msg)

    // While loop with counter
    let i = 0
    while i < 5 {
        print(i)
        i = i + 1
    }

    // If/else if/else branching
    let val = 42
    if val == 42 {
        print("found it")
    } else if val == 0 {
        print("zero")
    } else {
        print("other")
    }

    // String indexing
    let s = "abc"
    print(s[0])
    print(s[1])
    print(s[2])

    // Comparisons
    if 10 > 5 {
        print("10 > 5")
    }

    // String splitting
    let pair = "hello,world"
    let first = split(pair, 0)
    let second = split(pair, 1)
    print(first)
    print(second)

    // String prefix matching
    if startsWith("hello world", "hello") {
        print("starts with hello")
    }

    // Length and Fibonacci
    print(len("test"))
    print(fibonacci(10))

    // Character counting
    let vowels = countChars("self-hosting compiler", "o")
    print("vowels (o): " + vowels)
}
