package main

import "fmt"

type Item struct {
    Name           string
    Value          int
    Weight, Volume float64
}

type Result struct {
    Counts []int
    Sum    int
}

func min(a, b int) int {
    if a < b {
        return a
    }
    return b
}

func Knapsack(items []Item, index int, weight, volume float64) (best *Result) {
    if index == len(items) {
        return &Result{make([]int, len(items)), 0}
    }
    itemValue := items[index].Value
    itemWeight := items[index].Weight
    itemVolume := items[index].Volume
    maxCount := min(int(weight/itemWeight), int(volume/itemVolume))
    for count := 0; count <= maxCount; count++ {
        sol := Knapsack(items, index+1,
            weight-float64(count)*itemWeight,
            volume-float64(count)*itemVolume)
        if sol != nil {
            sol.Counts[index] = count
            sol.Sum += itemValue * count
            if best == nil || sol.Sum > best.Sum {
                best = sol
            }
        }
    }
    return
}

func main() {
    items := []Item{
        Item{"Panacea", 3000, 0.3, 0.025},
        Item{"Ichor",   1800, 0.2, 0.015},
        Item{"Gold",    2500, 2.0, 0.002},
    }
    var sumCount, sumValue int
    var sumWeight, sumVolume float64

    result := Knapsack(items, 0, 25, 0.25)

    for i := range result.Counts {
        fmt.Printf("%-8s x%3d  -> Weight: %4.1f  Volume: %5.3f  Value: %6d\n",
            items[i].Name, result.Counts[i], items[i].Weight*float64(result.Counts[i]),
            items[i].Volume*float64(result.Counts[i]), items[i].Value*result.Counts[i])

        sumCount += result.Counts[i]
        sumValue += items[i].Value * result.Counts[i]
        sumWeight += items[i].Weight * float64(result.Counts[i])
        sumVolume += items[i].Volume * float64(result.Counts[i])
    }

    fmt.Printf("TOTAL (%3d items) Weight: %4.1f  Volume: %5.3f  Value: %6d\n",
        sumCount, sumWeight, sumVolume, sumValue)
}
