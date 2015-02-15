package main

import (
    "fmt"
    "math"
)

// Task:  Formal power series type
//
// Go does not have a concept of numeric types other than the built in
// integers, floating points, and so on.  Nor does it have function or
// operator overloading, or operator defintion.  The type use to implement
// fps here is an interface with a single method, extract.
// While not named in the task description, extract is described in the
// WP article as "important."  In fact, by representing a way to index
// all of the coefficients of a fps, any type that implements the interface
// represents a formal power series.

type fps interface {
    extract(int) float64
}

// Task:  Operations on FPS
//
// Separate operations are implemented with separate extract methods.
// This requires each operation on the fps type to have a concrete type.
// Executing a fps operation is the act of instantiating the concrete type.
// This is implemented here with constructor functions that construct a
// new fps from fps arguments.

// Constructor functions are shown here as a group, followed by concrete
// type definitions and associated extract methods.

func one() fps {
    return &oneFps{}
}

func add(s1, s2 fps) fps {
    return &sum{s1: s1, s2: s2}
}

func sub(s1, s2 fps) fps {
    return &diff{s1: s1, s2: s2}
}

func mul(s1, s2 fps) fps {
    return &prod{s1: s1, s2: s2}
}

func div(s1, s2 fps) fps {
    return &quo{s1: s1, s2: s2}
}

func differentiate(s1 fps) fps {
    return &deriv{s1: s1}
}

func integrate(s1 fps) fps {
    return &integ{s1: s1}
}

// Example:  Mutually recursive defintion of sine and cosine.
// This is a constructor just as those above.  It is nullary and returns
// two fps.  Note sin and cos implemented as instances of other fps defined
// above, and so do not need new concrete types.  Note also the constant
// term of the integration fps provides the case that terminates recursion
// of the extract function.
func sinCos() (fps, fps) {
    sin := &integ{}
    cos := sub(one(), integrate(sin))
    sin.s1 = cos
    return sin, cos
}

// Following are type definitions and extract methods for fps operators
// (constructor functions) just defined.
//
// Goal:  lazy evaluation
//
// Go has no built in support for lazy evaluation, so we make it from
// scratch here.  Types contain, at a minimum, their fps operands and
// representation neccessary to implement lazy evaluation.  Typically
// this is a coefficient slice, although constant terms are not stored,
// so in the case of a constant fps, no slice is needed at all.
// Coefficients are generated only as they are requested.  Computed
// coefficients are stored in the slice and if requested subsequently,
// are returned immediately rather than recomputed.
//
// Types can also contain any other intermediate values useful for
// computing coefficients.

// Constant one:  A constant is a nullary function and no coefficent
// storage is needed so an empty struct is used for the type.
type oneFps struct{}

// The extract method implements the fps interface.  It simply has to
// return 1 for the first term and return 0 for all other terms.
func (*oneFps) extract(n int) float64 {
    if n == 0 {
        return 1
    }
    return 0
}

// Addition is a binary function so the sum type stores its two fps operands
// and its computed terms.
type sum struct {
    s      []float64
    s1, s2 fps
}

func (s *sum) extract(n int) float64 {
    for i := len(s.s); i <= n; i++ {
        s.s = append(s.s, s.s1.extract(i)+s.s2.extract(i))
    }
    return s.s[n]
}

// Subtraction and other binary operations are similar.
// (The common field definitions could be factored out with an embedded
// struct, but the clutter of the extra syntax required doesn't seem
// to be worthwhile.)
type diff struct {
    s      []float64
    s1, s2 fps
}

func (s *diff) extract(n int) float64 {
    for i := len(s.s); i <= n; i++ {
        s.s = append(s.s, s.s1.extract(i)-s.s2.extract(i))
    }
    return s.s[n]
}

type prod struct {
    s      []float64
    s1, s2 fps
}

func (s *prod) extract(n int) float64 {
    for i := len(s.s); i <= n; i++ {
        c := 0.
        for k := 0; k <= i; k++ {
            c += s.s1.extract(k) * s.s1.extract(n-k)
        }
        s.s = append(s.s, c)
    }
    return s.s[n]
}

// Note a couple of fields in addition to those of other binary operators.
// They simply optimize computations a bit.
type quo struct {
    s1, s2 fps
    inv    float64   // optimizes a divide
    c      []float64 // saves multiplications
    s      []float64
}

// WP formula.  Note the limitation s2[0] cannot be 0.  In this case
// the function returns NaN for all terms.  The switch statement catches
// this case and avoids storing a slice of all NaNs.
func (s *quo) extract(n int) float64 {
    switch {
    case len(s.s) > 0:
    case !math.IsInf(s.inv, 1):
        a0 := s.s2.extract(0)
        s.inv = 1 / a0
        if a0 != 0 {
            break
        }
        fallthrough
    default:
        return math.NaN()
    }
    for i := len(s.s); i <= n; i++ {
        c := 0.
        for k := 1; k <= i; k++ {
            c += s.s2.extract(k) * s.c[n-k]
        }
        c = s.s1.extract(i) - c*s.inv
        s.c = append(s.c, c)
        s.s = append(s.s, c*s.inv)
    }
    return s.s[n]
}

// Note differentiation and integration are unary so their types contain
// only a single fps operand.

type deriv struct {
    s   []float64
    s1  fps
}

func (s *deriv) extract(n int) float64 {
    for i := len(s.s); i <= n; {
        i++
        s.s = append(s.s, float64(i)*s.s1.extract(i))
    }
    return s.s[n]
}

type integ struct {
    s   []float64
    s1  fps
}

func (s *integ) extract(n int) float64 {
    if n == 0 {
        return 0 // constant term C=0
    }
    // with constant term handled, s starts at 1
    for i := len(s.s) + 1; i <= n; i++ {
        s.s = append(s.s, s.s1.extract(i-1)/float64(i))
    }
    return s.s[n-1]
}

// Demonstrate working sin, cos.
func main() {
    // Format several terms in a way that is easy to compare visually.
    partialSeries := func(f fps) (s string) {
        for i := 0; i < 6; i++ {
            s = fmt.Sprintf("%s %8.5f ", s, f.extract(i))
        }
        return
    }
    sin, cos := sinCos()
    fmt.Println("sin:", partialSeries(sin))
    fmt.Println("cos:", partialSeries(cos))
}
