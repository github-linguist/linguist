package main

import (
    "errors"
    "fmt"
    "reflect"
    "strconv"
    "strings"
    "unicode"
)

var input = `((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))`

func main() {
    fmt.Println("input:")
    fmt.Println(input)

    s, err := parseSexp(input)
    if err != nil {
        fmt.Println("error:", err)
        return
    }

    fmt.Println("\nparsed:")
    fmt.Println(s)

    fmt.Println("\nrepresentation:")
    s.dump(0)
}

// dynamic types for i are string, qString, int, float64, list, and error.
type sexp struct {
    i interface{}
}
type qString string
type list []sexp

func (s sexp) String() string {
    return fmt.Sprintf("%v", s.i)
}

func (q qString) String() string {
    return strconv.Quote(string(q))
}

func (l list) String() string {
    if len(l) == 0 {
        return "()"
    }
    b := fmt.Sprintf("(%v", l[0])
    for _, s := range l[1:] {
        b = fmt.Sprintf("%s %v", b, s)
    }
    return b + ")"
}

// parseSexp parses a string into a Go representation of an s-expression.
//
// Quoted strings go from one " to the next.  There is no escape character,
// all characters except " are valid.
//
// Otherwise atoms are any string of characters between any of '(', ')',
// '"', or white space characters.  If the atom parses as a Go int type
// using strconv.Atoi, it is taken as int; if it parses as a Go float64
// type using strconv.ParseFloat, it is taken as float64; otherwise it is
// taken as an unquoted string.
//
// Unmatched (, ), or " are errors.
// An empty or all whitespace input string is an error.
// Left over text after the sexp is an error.
//
// An empty list is a valid sexp, but there is no nil, no cons, no dot.
func parseSexp(s string) (sexp, error) {
    s1, rem := ps2(s, -1)
    if err, isErr := s1.i.(error); isErr {
        return sexp{}, err
    }
    if rem > "" {
        return s1, errors.New("Left over text: " + rem)
    }
    return s1, nil
}

// recursive.  n = -1 means not parsing a list.  n >= 0 means the number
// of list elements parsed so far.  string result is unparsed remainder
// of the input string s0.
func ps2(s0 string, n int) (x sexp, rem string) {
    tok, s1 := gettok(s0)
    switch t := tok.(type) {
    case error:
        return sexp{tok}, s1
    case nil: // this is also an error
        if n < 0 {
            return sexp{errors.New("blank input string")}, s0
        } else {
            return sexp{errors.New("unmatched (")}, ""
        }
    case byte:
        switch {
        case t == '(':
            x, s1 = ps2(s1, 0) // x is a list
            if _, isErr := x.i.(error); isErr {
                return x, s0
            }
        case n < 0:
            return sexp{errors.New("unmatched )")}, ""
        default:
            // found end of list.  allocate space for it.
            return sexp{make(list, n)}, s1
        }
    default:
        x = sexp{tok} // x is an atom
    }
    if n < 0 {
        // not in a list, just return the s-expression x
        return x, s1
    }
    // in a list.  hold on to x while we parse the rest of the list.
    l, s1 := ps2(s1, n+1)
    // result l is either an error or the allocated list, not completely
    // filled in yet.
    if _, isErr := l.i.(error); !isErr {
        // as long as no errors, drop x into its place in the list
        l.i.(list)[n] = x
    }
    return l, s1
}

// gettok gets one token from string s.
// return values are the token and the remainder of the string.
// dynamic type of tok indicates result:
// nil:  no token.  string was empty or all white space.
// byte:  one of '(' or ')'
// otherwise string, qString, int, float64, or error.
func gettok(s string) (tok interface{}, rem string) {
    s = strings.TrimSpace(s)
    if s == "" {
        return nil, ""
    }
    switch s[0] {
    case '(', ')':
        return s[0], s[1:]
    case '"':
        if i := strings.Index(s[1:], `"`); i >= 0 {
            return qString(s[1 : i+1]), s[i+2:]
        }
        return errors.New(`unmatched "`), s
    }
    i := 1
    for i < len(s) && s[i] != '(' && s[i] != ')' && s[i] != '"' &&
        !unicode.IsSpace(rune(s[i])) {
        i++
    }
    if j, err := strconv.Atoi(s[:i]); err == nil {
        return j, s[i:]
    }
    if f, err := strconv.ParseFloat(s[:i], 64); err == nil {
        return f, s[i:]
    }
    return s[:i], s[i:]
}

func (s sexp) dump(i int) {
    fmt.Printf("%*s%v: ", i*3, "", reflect.TypeOf(s.i))
    if l, isList := s.i.(list); isList {
        fmt.Println(len(l), "elements")
        for _, e := range l {
            e.dump(i + 1)
        }
    } else {
        fmt.Println(s.i)
    }
}
