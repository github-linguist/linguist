package main

import "fmt"

// function required by task
func degrees2compasspoint(h float32) string {
    return compassPoint[cpx(h)]
}

// cpx returns integer index from 0 to 31 corresponding to compass point.
// input heading h is in degrees.  Note this index is a zero-based index
// suitable for indexing into the table of printable compass points,
// and is not the same as the index specified to be printed in the output.
func cpx(h float32) int {
    x := int(h/11.25+.5) % 32
    if x < 0 {
        x += 32
    }
    return x
}

// printable compass points
var compassPoint = []string{
    "North",
    "North by east",
    "North-northeast",
    "Northeast by north",
    "Northeast",
    "Northeast by east",
    "East-northeast",
    "East by north",
    "East",
    "East by south",
    "East-southeast",
    "Southeast by east",
    "Southeast",
    "Southeast by south",
    "South-southeast",
    "South by east",
    "South",
    "South by west",
    "South-southwest",
    "Southwest by south",
    "Southwest",
    "Southwest by west",
    "West-southwest",
    "West by south",
    "West",
    "West by north",
    "West-northwest",
    "Northwest by west",
    "Northwest",
    "Northwest by north",
    "North-northwest",
    "North by west",
}

func main() {
    fmt.Println("Index  Compass point         Degree")
    for i, h := range []float32{0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5,
        84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75,
        185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0,
        286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38} {
        index := i%32 + 1 // printable index computed per pseudocode
        fmt.Printf("%4d   %-19s %7.2f°\n", index, degrees2compasspoint(h), h)
    }
}
