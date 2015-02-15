package main

import (
    "fmt"
    "image"
    "image/color"
    "image/draw"
    "image/png"
    "os"
)

func main() {
    const order = 8
    const width = 1 << order
    const margin = 10
    bounds := image.Rect(-margin, -margin, width+2*margin, width+2*margin)
    im := image.NewGray(bounds)
    gBlack := color.Gray{0}
    gWhite := color.Gray{255}
    draw.Draw(im, bounds, image.NewUniform(gWhite), image.ZP, draw.Src)

    for y := 0; y < width; y++ {
        for x := 0; x < width; x++ {
            if x&y == 0 {
                im.SetGray(x, y, gBlack)
            }
        }
    }
    f, err := os.Create("sierpinski.png")
    if err != nil {
        fmt.Println(err)
        return
    }
    if err = png.Encode(f, im); err != nil {
        fmt.Println(err)
    }
    if err = f.Close(); err != nil {
        fmt.Println(err)
    }
}
