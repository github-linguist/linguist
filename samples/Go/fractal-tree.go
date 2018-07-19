package main

// Files required to build supporting package raster are found in:
// * Bitmap
// * Grayscale image
// * Xiaolin Wu's line algorithm
// * Write a PPM file

import (
    "math"
    "raster"
)

const (
    width  = 400
    height = 300
    depth  = 8
    angle  = 12
    length = 50
    frac   = .8
)

func main() {
    g := raster.NewGrmap(width, height)
    ftree(g, width/2, height*9/10, length, 0, depth)
    g.Bitmap().WritePpmFile("ftree.ppm")
}

func ftree(g *raster.Grmap, x, y, distance, direction float64, depth int) {
    x2 := x + distance*math.Sin(direction*math.Pi/180)
    y2 := y - distance*math.Cos(direction*math.Pi/180)
    g.AaLine(x, y, x2, y2)
    if depth > 0 {
        ftree(g, x2, y2, distance*frac, direction-angle, depth-1)
        ftree(g, x2, y2, distance*frac, direction+angle, depth-1)
    }
}
