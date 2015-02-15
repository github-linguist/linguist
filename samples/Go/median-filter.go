package main

// Files required to build supporting package raster are found in:
// * Bitmap
// * Grayscale image
// * Read a PPM file
// * Write a PPM file

import (
    "fmt"
    "raster"
)

var g0, g1 *raster.Grmap
var ko [][]int
var kc []uint16
var mid int

func init() {
    // hard code box of 9 pixels
    ko = [][]int{
        {-1, -1}, {0, -1}, {1, -1},
        {-1,  0}, {0,  0}, {1,  0},
        {-1,  1}, {0,  1}, {1,  1}}
    kc = make([]uint16, len(ko))
    mid = len(ko) / 2
}

func main() {
    // Example file used here is Lenna50.jpg from the task "Percentage
    // difference between images" converted with with the command
    // convert Lenna50.jpg -colorspace gray Lenna50.ppm
    // It shows very obvious compression artifacts when viewed at higher
    // zoom factors.
    b, err := raster.ReadPpmFile("Lenna50.ppm")
    if err != nil {
        fmt.Println(err)
        return
    }
    g0 = b.Grmap()
    w, h := g0.Extent()
    g1 = raster.NewGrmap(w, h)
    for y := 0; y < h; y++ {
        for x := 0; x < w; x++ {
            g1.SetPx(x, y, median(x, y))
        }
    }
    // side by side comparison with input file shows compression artifacts
    // greatly smoothed over, although at some loss of contrast.
    err = g1.Bitmap().WritePpmFile("median.ppm")
    if err != nil {
        fmt.Println(err)
    }
}

func median(x, y int) uint16 {
    var n int
    // construct sorted list as pixels are read.  insertion sort can't be
    // beat for a small number of items, plus there would be lots of overhead
    // just to get numbers in and out of a library sort routine.
    for _, o := range ko {
        // read a pixel of the kernel
        c, ok := g0.GetPx(x+o[0], y+o[1])
        if !ok {
            continue
        }
        // insert it in sorted order
        var i int
        for ; i < n; i++ {
            if c < kc[i] {
                for j := n; j > i; j-- {
                    kc[j] = kc[j-1]
                }
                break
            }
        }
        kc[i] = c
        n++
    }
    // compute median from sorted list
    switch {
    case n == len(kc): // the usual case, pixel with complete neighborhood
        return kc[mid]
    case n%2 == 1: // edge case, odd number of pixels
        return kc[n/2]
    }
    // else edge case, even number of pixels
    m := n / 2
    return (kc[m-1] + kc[m]) / 2
}
