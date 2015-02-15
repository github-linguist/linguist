// Example serves as an example but does nothing useful.
//
// A package comment preceeds the package clause and explains the purpose
// of the package.
package example

// Exported variables.
var (
    // lookie
    X, Y, Z int // popular names
)

/* XP does nothing.

Here's a block comment. */
func XP() { // here we go!
    // comments inside
}

// Non-exported.
func nonXP() {}

// Doc not extracted.

var MEMEME int
