-- As a simple example, this function computes the average of an array of 64-bit floating-point numbers:
let average (xs: []f64) = reduce (+) 0.0 xs / r64 (length xs)
