// This program follows the license outlined, here: https://github.com/AleoHQ/leo/blob/mainnet/LICENSE.md

program twoadicity.aleo {
    // This function calculates the number of powers of two ("twoadicity")
    // in the prime factorization of the input number `n`.
    transition main(public n: field) -> u8 {
        let remaining_n: field = n;
        let powers_of_two: u8 = 0u8;
        // Since field ints are 253 bits or fewer, any number in the field
        // will have at most 252 powers of two in its prime factoring.
        for i:u8 in 0u8..252u8 {
            if is_even_and_nonzero(remaining_n) {
                remaining_n = remaining_n / 2field;
                powers_of_two = powers_of_two + 1u8;
            }
        }
        return powers_of_two;
    }

    /* We define the is_even predicate on fields as follows.
       If n is even and nonzero, clearly n/2 < n.
       If n is odd, n-p is a field-equivalent negative number that is even, and
       (n-p)/2 is a field-equivalent negative number closer to 0, greater than n-p.
       If we add p to both of these negative numbers, we have
       n/2 = (n-p)/2 + p = (n+p)/2 is greater than n and still less than p.
    */
    function is_even_and_nonzero (n: field) -> bool {
        return n / 2field < n;
    }
}
