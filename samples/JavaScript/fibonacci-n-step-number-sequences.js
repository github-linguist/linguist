function fib(arity, len) {
    return nacci(nacci([1,1], arity, arity), arity, len);
}

function lucas(arity, len) {
    return nacci(nacci([2,1], arity, arity), arity, len);
}

function nacci(a, arity, len) {
    while (a.length < len) {
        var sum = 0;
        for (var i = Math.max(0, a.length - arity); i < a.length; i++)
            sum += a[i];
        a.push(sum);
    }
    return a;
}

function main() {
    for (var arity = 2; arity <= 10; arity++)
        console.log("fib(" + arity + "): " + fib(arity, 15));
    for (var arity = 2; arity <= 10; arity++)
        console.log("lucas(" + arity + "): " + lucas(arity, 15));
}

main();
