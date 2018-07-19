function hailstone (n) {
    var seq = [n];
    while (n > 1) {
        n = n % 2 ? 3 * n + 1 : n / 2;
        seq.push(n);
    }
    return seq;
}

// task 2: verify the sequence for n = 27
var h = hailstone(27), hLen = h.length;
print("sequence 27 is (" + h.slice(0, 4).join(", ") + " ... "
    + h.slice(hLen - 4, hLen).join(", ") + "). length: " + hLen);

// task 3: find the longest sequence for n < 100000
for (var n, max = 0, i = 100000; --i;) {
    var seq = hailstone(i), sLen = seq.length;
    if (sLen > max) {
        n = i;
        max = sLen;
    }
}
print("longest sequence: " + max + " numbers for starting point " + n);
