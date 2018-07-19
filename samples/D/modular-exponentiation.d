import std.stdio, std.bigint;

BigInt powMod(BigInt base, BigInt exponent, BigInt modulus)
in {
   assert(exponent >= 0);
} body {
    BigInt result = 1;
    while (exponent > 0) {
        if (exponent % 2 == 1)
            result = (result * base) % modulus;
        exponent /= 2;
        base = base ^^ 2 % modulus;
    }
    return result;
}

void main() {
    powMod(BigInt("29883481620585741369158914214988194" ~
                  "66320163312926952423791023078876139"),
           BigInt("235139930337346448646612254452369009" ~
                  "4744975233415544072992656881240319"),
           BigInt(10) ^^ 40).writeln();
}
