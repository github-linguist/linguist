package main

import (
    "fmt"
    "image"
    "image/color"
    "image/png"
    "math"
    "os"
)

type vector [3]float64

func normalize(v *vector) {
    invLen := 1 / math.Sqrt(dot(v, v))
    v[0] *= invLen
    v[1] *= invLen
    v[2] *= invLen
}

func dot(x, y *vector) float64 {
    return x[0]*y[0] + x[1]*y[1] + x[2]*y[2]
}

func drawSphere(r int, k, amb float64, dir *vector) *image.Gray {
    w, h := r*4, r*3
    img := image.NewGray(image.Rect(-w/2, -h/2, w/2, h/2))
    vec := new(vector)
    for x := -r; x < r; x++ {
        for y := -r; y < r; y++ {
            if z := r*r - x*x - y*y; z >= 0 {
                vec[0] = float64(x)
                vec[1] = float64(y)
                vec[2] = math.Sqrt(float64(z))
                normalize(vec)
                s := dot(dir, vec)
                if s < 0 {
                    s = 0
                }
                lum := 255 * (math.Pow(s, k) + amb) / (1 + amb)
                if lum < 0 {
                    lum = 0
                } else if lum > 255 {
                    lum = 255
                }
                img.SetGray(x, y, color.Gray{uint8(lum)})
            }
        }
    }
    return img
}

func main() {
    dir := &vector{-30, -30, 50}
    normalize(dir)
    img := drawSphere(200, 1.5, .2, dir)
    f, err := os.Create("sphere.png")
    if err != nil {
        fmt.Println(err)
        return
    }
    if err = png.Encode(f, img); err != nil {
        fmt.Println(err)
    }
    if err = f.Close(); err != nil {
        fmt.Println(err)
    }
}
