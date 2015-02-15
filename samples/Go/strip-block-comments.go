package main

import (
    "fmt"
    "strings"
)

// idiomatic to name a function newX that allocates an object, initializes it,
// and returns it ready to use.  the object in this case is a closure.
func newStripper(start, end string) func(string) string {
    // default to c-style block comments
    if start == "" || end == "" {
        start, end = "/*", "*/"
    }
    // closes on variables start, end.
    return func(source string) string {
        for {
            cs := strings.Index(source, start)
            if cs < 0 {
                break
            }
            ce := strings.Index(source[cs+2:], end)
            if ce < 0 {
                break
            }
            source = source[:cs] + source[cs+ce+4:]
        }
        return source
    }
}

func main() {
    // idiomatic is that zero values indicate to use meaningful defaults
    stripC := newStripper("", "")

    // strip function now defined and can be called any number of times
    // without respecifying delimiters
    fmt.Println(stripC(`  /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }`))
}
