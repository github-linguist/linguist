// This program follows the license outlined, here: https://github.com/ProvableHQ/leo/blob/mainnet/LICENSE.md

program fibonacci.aleo {
    // This calculates the n-th fibonacci number (up to 64th)
    transition fibonacci(public n: u8) -> u128 {
        assert(n <= 64u8);

        let f0: u128 = 0u128;
        let f1: u128 = 1u128;
        let c: u8 = 0u8;

        let z: u8 = reverse_bits(n);

        for i:u8 in 0u8..8u8 {
            if n > 0u8 {
                let f2i1: u128 = f1 * f1 + f0 * f0;
                let f2i: u128 =  f0 * (2u128 * f1 - f0);
                if z & 1u8.shl(c) == 0u8 {
                    f0 = f2i;
                    f1 = f2i1;
                } else {
                    f0 = f2i1;
                    f1 = f2i + f2i1;
                }
                c = c + 1u8;
                n = n >> 1u8;
            }
        }

        return f0;
    }

    function reverse_bits(n: u8) -> u8 {
        let reverse: u8 = 0u8;

        for i:u8 in 0u8..8u8 {
            if n > 0u8 {
                reverse = reverse << 1u8;

                if n & 1u8 == 1u8 {
                    reverse ^= 1u8;
                }

                n = n >> 1u8;
            }
        }

        return reverse;
    }
}
