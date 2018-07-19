string toRoman(int n) pure nothrow
in {
    assert(n < 5000);
} body {
    static immutable weights = [1000, 900, 500, 400, 100, 90,
                                50, 40, 10, 9, 5, 4, 1];
    static immutable symbols = ["M","CM","D","CD","C","XC","L",
                                "XL","X","IX","V","IV","I"];

    string roman;
    foreach (i, w; weights) {
        while (n >= w) {
            roman ~= symbols[i];
            n -= w;
        }
        if (n == 0)
            break;
    }
    return roman;
} unittest {
    assert(toRoman(455)  == "CDLV");
    assert(toRoman(3456) == "MMMCDLVI");
    assert(toRoman(2488) == "MMCDLXXXVIII");
}

void main() {}
