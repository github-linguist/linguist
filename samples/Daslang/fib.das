// from: https://daslang.io/#look

def fibR(n) {
    if (n < 2) {
        return n;
    } else {
        return fibR(n - 1) + fibR(n - 2);
    }
}
def fibI(n) {
    var last = 0;
    var cur = 1;
    for i in range(0, n-1); {
        let tmp = cur;
        cur += last;
        last = tmp;
    }
    return cur;
}