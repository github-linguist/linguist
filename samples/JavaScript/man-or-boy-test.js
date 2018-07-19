function a(k, x1, x2, x3, x4, x5) {
    function b() {
        k = k - 1;
        return a(k, b, x1, x2, x3, x4);
    }
    return k <= 0 ? x4() + x5() : b();
}

// this uses lambda wrappers around the numeric arguments
function x(n) {
    return function () {
        return n;
    };
}
alert(a(10, x(1), x(-1), x(-1), x(1), x(0)));
