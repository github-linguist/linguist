package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

func getPi(numThrows int) float64 {
    inCircle := 0
    for i := 0; i < numThrows; i++ {
        //a square with a side of length 2 centered at 0 has
        //x and y range of -1 to 1
        randX := rand.Float64()*2 - 1 //range -1 to 1
        randY := rand.Float64()*2 - 1 //range -1 to 1
        //distance from (0,0) = sqrt((x-0)^2+(y-0)^2)
        dist := math.Hypot(randX, randY)
        if dist < 1 { //circle with diameter of 2 has radius of 1
            inCircle++
        }
    }
    return 4 * float64(inCircle) / float64(numThrows)
}

func main() {
    rand.Seed(time.Now().UnixNano())
    fmt.Println(getPi(10000))
    fmt.Println(getPi(100000))
    fmt.Println(getPi(1000000))
    fmt.Println(getPi(10000000))
    fmt.Println(getPi(100000000))
}
