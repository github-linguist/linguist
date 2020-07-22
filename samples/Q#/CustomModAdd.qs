// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT License.

namespace Microsoft.Quantum.Numerics.Samples {
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Arithmetic;
    open Microsoft.Quantum.Diagnostics;
    open Microsoft.Quantum.Arrays;

    /// # Summary
    /// Tests a modular addition similar to the one in Fig. 4
    /// of https://arxiv.org/pdf/quant-ph/9511018v1.pdf.
    ///
    /// # Input
    /// ## inputs1
    /// List of integers to use for the first number
    /// ## inputs2
    /// List of integers to use for the second number
    /// ## modulus
    /// Modulus used when adding each pair of numbers
    /// ## numBits
    /// Number of bits to use to represent each number
    operation CustomModAdd(inputs1 : Int[], inputs2 : Int[], modulus : Int,
                            numBits : Int) : Int[]
    {
        EqualityFactI(Length(inputs1), Length(inputs2),
                      "Input arrays need to be of equal length.");
        mutable results = new Int[Length(inputs1)];
        for (i in IndexRange(inputs1)) {
            let input1 = inputs1[i];
            let input2 = inputs2[i];
            using ((xQubits, yQubits, mQubits, tmp, ctrl) = (Qubit[numBits], Qubit[numBits], Qubit[numBits], Qubit(), Qubit())) {
                let x = LittleEndian(xQubits);
                let y = LittleEndian(yQubits);
                let m = LittleEndian(mQubits);
                // initialize inputs
                ApplyXorInPlace(input1, x);
                ApplyXorInPlace(input2, y);
                ApplyXorInPlace(modulus, m);

                // first, add the two inputs
                AddI(x, y);
                // if the result is greater than or equal to the modulus,
                // we have to subtract `modulus` from the result:
                let yc = LittleEndian(yQubits + [tmp]);
                (Adjoint AddI)(m, yc);
                within {
                    CNOT(tmp, ctrl);
                } apply {
                    // we should not have subtracted m if there is overflow:
                    Controlled AddI([ctrl], (m, yc));
                    // now, uncompute temporary qubits:
                    Adjoint AddI(x, yc);
                }
                AddI(x, yc);
                X(ctrl);

                set results w/= i <- MeasureInteger(y);
                ResetAll(xQubits + yQubits + mQubits);
            }
        }
        return results;
    }
}
