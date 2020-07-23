// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT License.
namespace Microsoft.Quantum.Samples.IntegerFactorization {
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Arithmetic;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Math;
    open Microsoft.Quantum.Oracles;
    open Microsoft.Quantum.Characterization;
    open Microsoft.Quantum.Diagnostics;

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // Introduction ///////////////////////////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////////////////////////////////////////

    // This sample contains Q# code implementing Shor's quantum algorithm for
    // factoring integers. The underlying modular arithmetic is implemented
    // in phase encoding, based on a paper by Stephane Beauregard who gave a
    // quantum circuit for factoring n-bit numbers that needs 2n+3 qubits and
    // O(n³log(n)) elementary quantum gates.

    /// # Summary
    /// Uses Shor's algorithm to factor the parameter `number`
    ///
    /// # Input
    /// ## number
    /// A semiprime integer to be factored
    /// ## useRobustPhaseEstimation
    /// If set to true, we use Microsoft.Quantum.Characterization.RobustPhaseEstimation and
    /// Microsoft.Quantum.Characterization.QuantumPhaseEstimation otherwise
    ///
    /// # Output
    /// Pair of numbers p > 1 and q > 1 such that p⋅q = `number`
    operation FactorSemiprimeInteger(number : Int, useRobustPhaseEstimation : Bool) 
    : (Int, Int) {
        // First check the most trivial case, if the provided number is even
        if (number % 2 == 0) {
            Message("An even number has been given; 2 is a factor.");
            return (number / 2, 2);
        }
        // These mutables will keep track of if we found the factors,
        // and if so, what they are. The default value for the factors
        // is (1,1).
        mutable foundFactors = false;
        mutable factors = (1, 1);

        repeat {
            // Next try to guess a number co-prime to `number`
            // Get a random integer in the interval [1,number-1]
            let generator = RandomInt(number - 2) + 1;

            // Check if the random integer indeed co-prime using
            // Microsoft.Quantum.Math.IsCoprimeI.
            // If true use Quantum algorithm for Period finding.
            if (IsCoprimeI(generator, number)) {

                // Print a message using Microsoft.Quantum.Intrinsic.Message
                // indicating that we are doing something quantum.
                Message($"Estimating period of {generator}");

                // Call Quantum Period finding algorithm for
                // `generator` mod `number`.
                // Here we have a choice which Phase Estimation algorithm to use.
                let period = EstimatePeriod(generator, number, useRobustPhaseEstimation);

                // Set the flag and factors values if the continued fractions
                // classical algorithm succeeds.
                set (foundFactors, factors) = MaybeFactorsFromPeriod(number, generator, period);
            }
            // In this case, we guessed a divisor by accident.
            else {
                // Find a divisor using Microsoft.Quantum.Math.GreatestCommonDivisorI
                let gcd = GreatestCommonDivisorI(number, generator);
                
                // Don't forget to tell the user that we were lucky and didn't do anything
                // quantum by using Microsoft.Quantum.Intrinsic.Message.
                Message($"We have guessed a divisor of {number} to be {gcd} by accident.");

                // Set the flag `foundFactors` to true, indicating that we succeeded in finding
                // factors.
                set foundFactors = true;
                set factors = (gcd, number / gcd);
            }
        } 
        until (foundFactors)
        fixup {
            Message("The estimated period did not yield a valid factor, trying again.");
        }
        
        // Return the factorization
        return factors;
    }

    /// # Summary
    /// Interprets `target` as encoding unsigned little-endian integer k
    /// and performs transformation |k⟩ ↦ |gᵖ⋅k mod N ⟩ where
    /// p is `power`, g is `generator` and N is `modulus`.
    ///
    /// # Input
    /// ## generator
    /// The unsigned integer multiplicative order ( period )
    /// of which is being estimated. Must be co-prime to `modulus`.
    /// ## modulus
    /// The modulus which defines the residue ring Z mod `modulus`
    /// in which the multiplicative order of `generator` is being estimated.
    /// ## power
    /// Power of `generator` by which `target` is multiplied.
    /// ## target
    /// Register interpreted as LittleEndian which is multiplied by
    /// given power of the generator. The multiplication is performed modulo
    /// `modulus`.
    operation ApplyOrderFindingOracle(
        generator : Int, modulus : Int, power : Int, target : Qubit[]
    )
    : Unit
    is Adj + Ctl {
        // Check that the parameters satisfy the requirements.
        Fact(IsCoprimeI(generator, modulus), "`generator` and `modulus` must be co-prime");

        // The oracle we use for order finding essentially wraps
        // Microsoft.Quantum.Arithmetic.MultiplyByModularInteger operation
        // that implements |x⟩ ↦ |x⋅a mod N ⟩.
        // We also use Microsoft.Quantum.Math.ExpModI to compute a by which
        // x must be multiplied.
        // Also note that we interpret target as unsigned integer
        // in little-endian encoding by using Microsoft.Quantum.Arithmetic.LittleEndian
        // type.
        MultiplyByModularInteger(ExpModI(generator, power, modulus), modulus, LittleEndian(target));
    }

    /// # Summary
    /// Finds a multiplicative order of the generator
    /// in the residue ring Z mod `modulus`.
    ///
    /// # Input
    /// ## generator
    /// The unsigned integer multiplicative order ( period )
    /// of which is being estimated. Must be co-prime to `modulus`.
    /// ## modulus
    /// The modulus which defines the residue ring Z mod `modulus`
    /// in which the multiplicative order of `generator` is being estimated.
    /// ## useRobustPhaseEstimation
    /// If set to true, we use Microsoft.Quantum.Characterization.RobustPhaseEstimation and
    /// Microsoft.Quantum.Characterization.QuantumPhaseEstimation
    ///
    /// # Output
    /// The period ( multiplicative order ) of the generator mod `modulus`
    operation EstimatePeriod(
        generator : Int, modulus : Int, useRobustPhaseEstimation : Bool
    ) 
    : Int {
        // Here we check that the inputs to the EstimatePeriod operation are valid.
        Fact(IsCoprimeI(generator, modulus), "`generator` and `modulus` must be co-prime");

        // The variable that stores the divisor of the generator period found so far.
        mutable result = 1;

        // Number of bits in the modulus with respect to which we are estimating the period.
        let bitsize = BitSizeI(modulus);

        // The EstimatePeriod operation estimates the period r by finding an
        // approximation k/2^(bits precision) to a fraction s/r, where s is some integer.
        // Note that if s and r have common divisors we will end up recovering a divisor of r
        // and not r itself. However, if we recover enough divisors of r
        // we recover r itself pretty soon.

        // Number of bits of precision with which we need to estimate s/r to recover period r.
        // using continued fractions algorithm.
        let bitsPrecision = 2 * bitsize + 1;

        // A variable that stores our current estimate for the frequency
        // of the form s/r. 
        mutable frequencyEstimate = 0;

        repeat {

            set frequencyEstimate = EstimateFrequency(
                generator, modulus, useRobustPhaseEstimation, bitsize 
            );

            if (frequencyEstimate != 0) {
                set result = PeriodFromFrequency(modulus,frequencyEstimate, bitsPrecision, result);
            }
            else {
                Message("The estimated frequency was 0, trying again.");
            }
        }
        until(ExpModI(generator, result, modulus) == 1)
        fixup {
            Message("The estimated period from continued fractions failed, trying again.");
        }

        return result;
    }

    /// # Summary
    /// Estimates the frequency of a generator
    /// in the residue ring Z mod `modulus`.
    ///
    /// # Input
    /// ## generator
    /// The unsigned integer multiplicative order ( period )
    /// of which is being estimated. Must be co-prime to `modulus`.
    /// ## modulus
    /// The modulus which defines the residue ring Z mod `modulus`
    /// in which the multiplicative order of `generator` is being estimated.
    /// ## useRobustPhaseEstimation
    /// If set to true, we use Microsoft.Quantum.Characterization.RobustPhaseEstimation else
    /// this operation uses Microsoft.Quantum.Characterization.QuantumPhaseEstimation
    /// ## bitsize
    /// Number of bits needed to represent the modulus.
    ///
    /// # Output
    /// The numerator k of dyadic fraction k/2^bitsPrecision
    /// approximating s/r.
    operation EstimateFrequency(
        generator : Int, 
        modulus : Int,
        useRobustPhaseEstimation : Bool, 
        bitsize : Int
    )
    : Int {
        mutable frequencyEstimate = 0;
        let bitsPrecision =  2 * bitsize + 1;
        
        // Allocate qubits for the superposition of eigenstates of
        // the oracle that is used in period finding.
        using (eigenstateRegister = Qubit[bitsize]) {

            // Initialize eigenstateRegister to 1, which is a superposition of
            // the eigenstates we are estimating the phases of.
            // We first interpret the register as encoding an unsigned integer
            // in little endian encoding.
            let eigenstateRegisterLE = LittleEndian(eigenstateRegister);
            ApplyXorInPlace(1, eigenstateRegisterLE);

            // An oracle of type Microsoft.Quantum.Oracles.DiscreteOracle
            // that we are going to use with phase estimation methods below.
            let oracle = DiscreteOracle(ApplyOrderFindingOracle(generator, modulus, _, _));

            if (useRobustPhaseEstimation) {

                // Use Microsoft.Quantum.Characterization.RobustPhaseEstimation to estimate s/r.
                // RobustPhaseEstimation needs only one extra qubit, but requires
                // several calls to the oracle.
                let phase = RobustPhaseEstimation(bitsPrecision, oracle, eigenstateRegisterLE!);
                
                // Compute the numerator k of dyadic fraction k/2^bitsPrecision
                // approximating s/r. Note that phase estimation projects on the eigenstate
                // corresponding to random s.
                set frequencyEstimate = Round(((phase * IntAsDouble(2 ^ bitsPrecision)) / 2.0) / PI());
            }
            else {
                // Use Microsoft.Quantum.Characterization.QuantumPhaseEstimation to estimate s/r.
                // When using QuantumPhaseEstimation we will need extra `bitsPrecision`
                // qubits
                using (register = Qubit[bitsPrecision]) {
                    let frequencyEstimateNumerator = LittleEndian(register);  

                    // The register that will contain the numerator k of
                    // dyadic fraction k/2^bitsPrecision. The numerator is an unsigned
                    // integer encoded in big-endian format. This is indicated by
                    // use of Microsoft.Quantum.Arithmetic.BigEndian type.
                    QuantumPhaseEstimation(
                        oracle, eigenstateRegisterLE!, LittleEndianAsBigEndian(frequencyEstimateNumerator)
                    ); 
                    
                    // Directly measure the numerator k of dyadic fraction k/2^bitsPrecision
                    // approximating s/r. Note that phase estimation project on
                    // the eigenstate corresponding to random s.
                    set frequencyEstimate = MeasureInteger(frequencyEstimateNumerator);
                }
            }
            
            // Return all the qubits used for oracle's eigenstate back to 0 state
            // using Microsoft.Quantum.Intrinsic.ResetAll.
            ResetAll(eigenstateRegister);
        }

        return frequencyEstimate;
    }

    /// # Summary
    /// Find the period of a number from an input frequency.
    ///
    /// # Input
    /// ## modulus
    /// The modulus which defines the residue ring Z mod `modulus`
    /// in which the multiplicative order of `generator` is being estimated.
    /// ## frequencyEstimate
    /// The frequency that we want to convert to a period. 
    /// ## bitsPrecision
    /// Number of bits of precision with which we need to 
    /// estimate s/r to recover period r using continued 
    /// fractions algorithm.
    /// ## currentDivisor
    /// The divisor of the generator period found so far.
    ///
    /// # Output
    /// The period as calculated from the estimated frequency via
    /// the continued fractions algorithm.
    ///
    /// # See Also
    /// - Microsoft.Quantum.Math.ContinuedFractionConvergentI
    function PeriodFromFrequency(
        modulus : Int, 
        frequencyEstimate : Int, 
        bitsPrecision : Int, 
        currentDivisor : Int
    )
    : Int {
        
        // Now we use Microsoft.Quantum.Math.ContinuedFractionConvergentI
        // function to recover s/r from dyadic fraction k/2^bitsPrecision.
        let (numerator, period) = (ContinuedFractionConvergentI(Fraction(frequencyEstimate, 2 ^ bitsPrecision), modulus))!;
        
        // ContinuedFractionConvergentI does not guarantee the signs of the numerator
        // and denominator. Here we make sure that both are positive using
        // AbsI.
        let (numeratorAbs, periodAbs) = (AbsI(numerator), AbsI(period));

        // Return the newly found divisor.
        // Uses Microsoft.Quantum.Math.GreatestCommonDivisorI function from Microsoft.Quantum.Math.
        return (periodAbs * currentDivisor) / GreatestCommonDivisorI(currentDivisor, periodAbs);
    }

    /// # Summary
    /// Tries to find the factors of `modulus` given a `period` and `generator`.
    ///
    /// # Input
    /// ## modulus
    /// The modulus which defines the residue ring Z mod `modulus`
    /// in which the multiplicative order of `generator` is being estimated.
    /// ## generator
    /// The unsigned integer multiplicative order ( period )
    /// of which is being estimated. Must be co-prime to `modulus`.
    /// ## period
    /// The estimated period ( multiplicative order ) of the generator mod `modulus`.
    ///
    /// # Output
    /// A tuple of a flag indicating whether factors were found successfully,
    /// and a pair of integers representing the factors that were found.
    /// Note that the second output is only meaningful when the first
    /// output is `true`.
    ///
    /// # See Also
    /// - Microsoft.Quantum.Math.GreatestCommonDivisorI
    function MaybeFactorsFromPeriod(modulus : Int, generator : Int, period : Int) 
    : (Bool, (Int, Int)) {
        // Period finding reduces to factoring only if period is even
        if (period % 2 == 0) {
            
            // Compute `generator` ^ `period/2` mod `number`
            // using Microsoft.Quantum.Math.ExpModI.
            let halfPower = ExpModI(generator, period / 2, modulus);

            // If we are unlucky, halfPower is just -1 mod N,
            // which is a trivial case and not useful for factoring.
            if (halfPower != modulus - 1) {

                // When the halfPower is not -1 mod N
                // halfPower-1 or halfPower+1 share non-trivial divisor with `number`.
                // We find a divisor Microsoft.Quantum.Math.GreatestCommonDivisorI.
                let factor = MaxI(
                    GreatestCommonDivisorI(halfPower - 1, modulus), 
                    GreatestCommonDivisorI(halfPower + 1, modulus)
                );
                
                // Add a flag that we found the factors, and return computed non-trivial factors.
                return (true, (factor, modulus / factor));
            } else {
                // Return a flag indicating we hit a trivial case and didn't get any factors.
                return (false, (1,1));
            }
        } else {
            // When period is odd we have to pick another generator to estimate
            // period of and start over.
            Message("Estimated period was odd, trying again.");
            return (false, (1,1));
        }
    }
}