package main

import (
    "fmt"
    "image"
    "image/color"
    "image/draw"
    "image/png"
    "os"
)

const (
    up = iota
    rt
    dn
    lt
)

func main() {
    bounds := image.Rect(0, 0, 100, 100)
    im := image.NewGray(bounds)
    gBlack := color.Gray{0}
    gWhite := color.Gray{255}
    draw.Draw(im, bounds, image.NewUniform(gWhite), image.ZP, draw.Src)
    pos := image.Point{50, 50}
    dir := up
    for pos.In(bounds) {
        switch im.At(pos.X, pos.Y).(color.Gray).Y {
        case gBlack.Y:
            im.SetGray(pos.X, pos.Y, gWhite)
            dir--
        case gWhite.Y:
            im.SetGray(pos.X, pos.Y, gBlack)
            dir++
        }
        if dir&1 == 1 {
            pos.X += 1 - dir&2
        } else {
            pos.Y -= 1 - dir&2
        }
    }
    f, err := os.Create("ant.png")
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
