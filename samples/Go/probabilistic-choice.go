package main

import (
    "fmt"
    "math/rand"
    "time"
)

type mapping struct {
    item string
    pr   float64
}

func main() {
    // input mapping
    m := []mapping{
        {"aleph", 1 / 5.},
        {"beth", 1 / 6.},
        {"gimel", 1 / 7.},
        {"daleth", 1 / 8.},
        {"he", 1 / 9.},
        {"waw", 1 / 10.},
        {"zayin", 1 / 11.},
        {"heth", 1759 / 27720.}} // adjusted so that probabilities add to 1

    // cumulative probability
    cpr := make([]float64, len(m)-1)
    var c float64
    for i := 0; i < len(m)-1; i++ {
        c += m[i].pr
        cpr[i] = c
    }

    // generate
    const samples = 1e6
    occ := make([]int, len(m))
    rand.Seed(time.Now().UnixNano())
    for i := 0; i < samples; i++ {
        r := rand.Float64()
        for j := 0; ; j++ {
            if r < cpr[j] {
                occ[j]++
                break
            }
            if j == len(cpr)-1 {
                occ[len(cpr)]++
                break
            }
        }
    }

    // report
    fmt.Println("  Item  Target   Generated")
    var totalTarget, totalGenerated float64
    for i := 0; i < len(m); i++ {
        target := m[i].pr
        generated := float64(occ[i]) / samples
        fmt.Printf("%6s  %8.6f  %8.6f\n", m[i].item, target, generated)
        totalTarget += target
        totalGenerated += generated
    }
    fmt.Printf("Totals  %8.6f  %8.6f\n", totalTarget, totalGenerated)
}
