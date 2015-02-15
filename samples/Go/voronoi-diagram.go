package main

import (
    "fmt"
    "image"
    "image/color"
    "image/draw"
    "image/png"
    "math/rand"
    "os"
    "time"
)

const (
    imageWidth  = 300
    imageHeight = 200
    nSites      = 10
)

func main() {
    writePngFile(generateVoronoi(randomSites()))
}

func generateVoronoi(sx, sy []int) image.Image {
    // generate a random color for each site
    sc := make([]color.NRGBA, nSites)
    for i := range sx {
        sc[i] = color.NRGBA{uint8(rand.Intn(256)), uint8(rand.Intn(256)),
            uint8(rand.Intn(256)), 255}
    }

    // generate diagram by coloring each pixel with color of nearest site
    img := image.NewNRGBA(image.Rect(0, 0, imageWidth, imageHeight))
    for x := 0; x < imageWidth; x++ {
        for y := 0; y < imageHeight; y++ {
            dMin := dot(imageWidth, imageHeight)
            var sMin int
            for s := 0; s < nSites; s++ {
                if d := dot(sx[s]-x, sy[s]-y); d < dMin {
                    sMin = s
                    dMin = d
                }
            }
            img.SetNRGBA(x, y, sc[sMin])
        }
    }
    // mark each site with a black box
    black := image.NewUniform(color.Black)
    for s := 0; s < nSites; s++ {
        draw.Draw(img, image.Rect(sx[s]-2, sy[s]-2, sx[s]+2, sy[s]+2),
            black, image.ZP, draw.Src)
    }
    return img
}

func dot(x, y int) int {
    return x*x + y*y
}

func randomSites() (sx, sy []int) {
    rand.Seed(time.Now().Unix())
    sx = make([]int, nSites)
    sy = make([]int, nSites)
    for i := range sx {
        sx[i] = rand.Intn(imageWidth)
        sy[i] = rand.Intn(imageHeight)
    }
    return
}

func writePngFile(img image.Image) {
    f, err := os.Create("voronoi.png")
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
