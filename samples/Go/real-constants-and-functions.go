import "math"

// e and pi defined as constants.
// In Go, that means they are not of a specific data type and can be used as float32 or float64.
math.E
math.Pi

// The following functions all take and return the float64 data type.

math.Sqrt(x) // square root--cube root also available (math.Cbrt)
// natural logarithm--log base 10, 2 also available (math.Log10, math.Log2)
// also available is log1p, the log of 1+x.  It is more accurate when x is near zero.
math.Log(x)
math.Exp(x) // exponential--exp base 10, 2 also available (math.Pow10, math.Exp2)
math.Abs(x) // absolute value
math.Floor(x) // floor
math.Ceil(x) // ceiling
math.Pow(x,y) // power
