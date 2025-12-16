// Created by Ilya Shchepetkov

MACHINE Leftpad

// Chars are modeled as some set. Strings are sequences of chars.
SETS
    Chars

OPERATIONS

result ← leftpad(cc, nn, ss) =
    // Precondition of the operation defines types of its parameters
    PRE cc ∈ Chars ∧
        nn ∈ ℕ ∧
        ss ∈ seq(Chars)
    THEN
        // ANY allows to introduce a local variable
        // and to constraint its possible values with a predicate
        ANY vv WHERE
            vv ∈ seq(Chars) ∧
            // 1. The length of the output is max(n, len(str))
            size(vv) = max({nn, size(ss)}) ∧
            // 2. The prefix of the output is padding characters and nothing but padding characters
            ∀ii . (ii ∈ ℕ ∧ ii ≥ 0 ∧ ii < nn - size(ss) ⇒ vv(ii + 1) = cc) ∧
            // 3. The suffix of the output is the original string.
            ∀ii . (ii ∈ ℕ ∧ ii ≥ 0 ∧ ii < size(ss) ⇒ vv(max({nn - size(ss), 0}) + ii + 1) = ss(ii + 1))
        THEN
            result := vv
        END
    END

END
