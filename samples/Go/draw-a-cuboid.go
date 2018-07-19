package main

import "fmt"

func cuboid(dx, dy, dz int) {
    fmt.Printf("cuboid %d %d %d:\n", dx, dy, dz)
    cubLine(dy+1, dx, 0, "+-")
    for i := 1; i <= dy; i++ {
        cubLine(dy-i+1, dx, i-1, "/ |")
    }
    cubLine(0, dx, dy, "+-|")
    for i := 4*dz - dy - 2; i > 0; i-- {
        cubLine(0, dx, dy, "| |")
    }
    cubLine(0, dx, dy, "| +")
    for i := 1; i <= dy; i++ {
        cubLine(0, dx, dy-i, "| /")
    }
    cubLine(0, dx, 0, "+-\n")
}

func cubLine(n, dx, dy int, cde string) {
    fmt.Printf("%*s", n+1, cde[:1])
    for d := 9*dx - 1; d > 0; d-- {
        fmt.Print(cde[1:2])
    }
    fmt.Print(cde[:1])
    fmt.Printf("%*s\n", dy+1, cde[2:])
}

func main() {
    cuboid(2, 3, 4)
    cuboid(1, 1, 1)
    cuboid(6, 2, 1)
}
