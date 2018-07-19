package main

import (
    "fmt"
    "image"
    "image/color"
    "image/draw"
    "image/png"
    "math"
    "math/rand"
    "os"
    "time"
)

type r2 struct {
    x, y float64
}

type r2c struct {
    r2
    c int // cluster number
}

// kmpp implements K-means++, satisfying the basic task requirement
func kmpp(k int, data []r2c) {
    kMeans(data, kmppSeeds(k, data))
}

// kmppSeeds is the ++ part.
// It generates the initial means for the k-means algorithm.
func kmppSeeds(k int, data []r2c) []r2 {
    s := make([]r2, k)
    s[0] = data[rand.Intn(len(data))].r2
    d2 := make([]float64, len(data))
    for i := 1; i < k; i++ {
        var sum float64
        for j, p := range data {
            _, dMin := nearest(p, s[:i])
            d2[j] = dMin * dMin
            sum += d2[j]
        }
        target := rand.Float64() * sum
        j := 0
        for sum = d2[0]; sum < target; sum += d2[j] {
            j++
        }
        s[i] = data[j].r2
    }
    return s
}

// nearest finds the nearest mean to a given point.
// return values are the index of the nearest mean, and the distance from
// the point to the mean.
func nearest(p r2c, mean []r2) (int, float64) {
    iMin := 0
    dMin := math.Hypot(p.x-mean[0].x, p.y-mean[0].y)
    for i := 1; i < len(mean); i++ {
        d := math.Hypot(p.x-mean[i].x, p.y-mean[i].y)
        if d < dMin {
            dMin = d
            iMin = i
        }
    }
    return iMin, dMin
}

// kMeans algorithm.  Lloyd's
func kMeans(data []r2c, mean []r2) {
    // initial assignment
    for i, p := range data {
        cMin, _ := nearest(p, mean)
        data[i].c = cMin
    }
    mLen := make([]int, len(mean))
    for {
        // update means
        for i := range mean {
            mean[i] = r2{}
            mLen[i] = 0
        }
        for _, p := range data {
            mean[p.c].x += p.x
            mean[p.c].y += p.y
            mLen[p.c]++
        }
        for i := range mean {
            inv := 1 / float64(mLen[i])
            mean[i].x *= inv
            mean[i].y *= inv
        }
        // make new assignments, count changes
        var changes int
        for i, p := range data {
            if cMin, _ := nearest(p, mean); cMin != p.c {
                changes++
                data[i].c = cMin
            }
        }
        if changes == 0 {
            return
        }
    }
}

// parameters for extra credit exercises
type ecParam struct {
    k          int
    nPoints    int
    xBox, yBox int
    stdv       int
}

// extra credit 1 and 2:
func main() {
    ec := &ecParam{6, 30000, 300, 200, 30}

    origin, data := genECData(ec)
    vis(ec, data, "origin")
    fmt.Println("Data set origins:")
    fmt.Println("    x      y")
    for _, o := range origin {
        fmt.Printf("%5.1f  %5.1f\n", o.x, o.y)
    }

    kmpp(ec.k, data)

    fmt.Println(
        "\nCluster centroids, mean distance from centroid, number of points:")
    fmt.Println("    x      y  distance  points")
    cent := make([]r2, ec.k)
    cLen := make([]int, ec.k)
    inv := make([]float64, ec.k)
    for _, p := range data {
        cent[p.c].x += p.x
        cent[p.c].y += p.y
        cLen[p.c]++
    }
    for i, iLen := range cLen {
        inv[i] = 1 / float64(iLen)
        cent[i].x *= inv[i]
        cent[i].y *= inv[i]
    }
    dist := make([]float64, ec.k)
    for _, p := range data {
        dist[p.c] += math.Hypot(p.x-cent[p.c].x, p.y-cent[p.c].y)
    }
    for i, iLen := range cLen {
        fmt.Printf("%5.1f  %5.1f  %8.1f  %6d\n",
            cent[i].x, cent[i].y, dist[i]*inv[i], iLen)
    }
    vis(ec, data, "clusters")
}

// genECData generates random data for extra credit tasks.
// k origin points are randomly selected in a bounding box.
// nPoints/k coordinates are then generated for each origin point.
// The x and y coordinates of the data are normally distributed
// with standard deviation stdv.  Thus data coordinates are not
// constrained to the origin box; they can range to +/- max float64.
func genECData(ec *ecParam) (orig []r2, data []r2c) {
    rand.Seed(time.Now().UnixNano())
    orig = make([]r2, ec.k)
    data = make([]r2c, ec.nPoints)
    for i, n := 0, 0; i < ec.k; i++ {
        x := rand.Float64() * float64(ec.xBox)
        y := rand.Float64() * float64(ec.yBox)
        orig[i] = r2{x, y}
        for j := ec.nPoints / ec.k; j > 0; j-- {
            data[n].x = rand.NormFloat64()*float64(ec.stdv) + x
            data[n].y = rand.NormFloat64()*float64(ec.stdv) + y
            data[n].c = i
            n++
        }
    }
    return
}

// vis writes a .png for extra credit 2.
func vis(ec *ecParam, data []r2c, fn string) {
    colors := make([]color.NRGBA, ec.k)
    for i := range colors {
        i3 := i * 3
        third := i3 / ec.k
        frac := uint8((i3 % ec.k) * 255 / ec.k)
        switch third {
        case 0:
            colors[i] = color.NRGBA{frac, 255 - frac, 0, 255}
        case 1:
            colors[i] = color.NRGBA{0, frac, 255 - frac, 255}
        case 2:
            colors[i] = color.NRGBA{255 - frac, 0, frac, 255}
        }
    }
    bounds := image.Rect(-ec.stdv, -ec.stdv, ec.xBox+ec.stdv, ec.yBox+ec.stdv)
    im := image.NewNRGBA(bounds)
    draw.Draw(im, bounds, image.NewUniform(color.White), image.ZP, draw.Src)
    fMinX := float64(bounds.Min.X)
    fMaxX := float64(bounds.Max.X)
    fMinY := float64(bounds.Min.Y)
    fMaxY := float64(bounds.Max.Y)
    for _, p := range data {
        imx := math.Floor(p.x)
        imy := math.Floor(float64(ec.yBox) - p.y)
        if imx >= fMinX && imx < fMaxX && imy >= fMinY && imy < fMaxY {
            im.SetNRGBA(int(imx), int(imy), colors[p.c])
        }
    }
    f, err := os.Create(fn + ".png")
    if err != nil {
        fmt.Println(err)
        return
    }
    err = png.Encode(f, im)
    if err != nil {
        fmt.Println(err)
    }
    err = f.Close()
    if err != nil {
        fmt.Println(err)
    }
}
