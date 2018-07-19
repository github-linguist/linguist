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

func (v *vector) normalize() {
    invLen := 1 / math.Sqrt(dot(v, v))
    v[0] *= invLen
    v[1] *= invLen
    v[2] *= invLen
}

func dot(x, y *vector) float64 {
    return x[0]*y[0] + x[1]*y[1] + x[2]*y[2]
}

type sphere struct {
    cx, cy, cz int
    r          int
}

func (s *sphere) hit(x, y int) (z1, z2 float64, hit bool) {
    x -= s.cx
    y -= s.cy
    if zsq := s.r*s.r - (x*x + y*y); zsq >= 0 {
        zsqrt := math.Sqrt(float64(zsq))
        return float64(s.cz) - zsqrt, float64(s.cz) + zsqrt, true
    }
    return 0, 0, false
}

func deathStar(pos, neg *sphere, k, amb float64, dir *vector) *image.Gray {
    w, h := pos.r*4, pos.r*3
    bounds := image.Rect(pos.cx-w/2, pos.cy-h/2, pos.cx+w/2, pos.cy+h/2)
    img := image.NewGray(bounds)
    vec := new(vector)
    for y, yMax := pos.cy-pos.r, pos.cy+pos.r; y <= yMax; y++ {
        for x, xMax := pos.cx-pos.r, pos.cx+pos.r; x <= xMax; x++ {
            zb1, zb2, hit := pos.hit(x, y)
            if !hit {
                continue
            }
            zs1, zs2, hit := neg.hit(x, y)
            if hit {
                if zs1 > zb1 {
                    hit = false
                } else if zs2 > zb2 {
                    continue
                }
            }
            if hit {
                vec[0] = float64(neg.cx - x)
                vec[1] = float64(neg.cy - y)
                vec[2] = float64(neg.cz) - zs2
            } else {
                vec[0] = float64(x - pos.cx)
                vec[1] = float64(y - pos.cy)
                vec[2] = zb1 - float64(pos.cz)
            }
            vec.normalize()
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
    return img
}

func main() {
    dir := &vector{20, -40, -10}
    dir.normalize()
    pos := &sphere{0, 0, 0, 120}
    neg := &sphere{-90, -90, -30, 100}

    img := deathStar(pos, neg, 1.5, .2, dir)
    f, err := os.Create("dstar.png")
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
