function pow(base, exp) {
    if (exp != Math.floor(exp))
        throw "exponent must be an integer";
    if (exp < 0)
        return 1 / pow(base, -exp);
    var ans = 1;
    while (exp > 0) {
        ans *= base;
        exp--;
    }
    return ans;
}
