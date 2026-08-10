// This program follows the license outlined, here: https://github.com/ProvableHQ/leo/blob/mainnet/LICENSE.md

program groups.aleo {
    // This function takes a group coordinate as input `a` and performs several operations which should output the `0group`.
    // Note that the operations can be called as associated functions on the `a` variable.

    transition main(a: group) -> group {
        // unary
        let b: group = a.double(); // 2a
        let c: group = b.neg();    // -2a

        // binary
        let d: group = (a * 2scalar).add(c);

        // generator
        let e: group = group::GEN;

        return d + e;
    }
}
