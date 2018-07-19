package raster

import (
    "math"
    "math/rand"
)

// Grmap parallels Bitmap, but with an element type of uint16
// in place of Pixel.
type Grmap struct {
    Comments   []string
    rows, cols int
    px         []uint16
    pxRow      [][]uint16
}

// NewGrmap constructor.
func NewGrmap(x, y int) (b *Grmap) {
    g := &Grmap{
        Comments: []string{creator}, // creator a const in bitmap source file
        rows:     y,
        cols:     x,
        px:       make([]uint16, x*y),
        pxRow:    make([][]uint16, y),
    }
    x0, x1 := 0, x
    for i := range g.pxRow {
        g.pxRow[i] = g.px[x0:x1]
        x0, x1 = x1, x1+x
    }
    return g
}

func (b *Grmap) Extent() (cols, rows int) {
    return b.cols, b.rows
}

func (g *Grmap) Fill(c uint16) {
    for i := range g.px {
        g.px[i] = c
    }
}

func (g *Grmap) SetPx(x, y int, c uint16) bool {
    defer func() { recover() }()
    g.pxRow[y][x] = c
    return true
}

func (g *Grmap) GetPx(x, y int) (uint16, bool) {
    defer func() { recover() }()
    return g.pxRow[y][x], true
}

// Grmap method of Bitmap, converts (color) Bitmap to (grayscale) Grmap
func (b *Bitmap) Grmap() *Grmap {
    g := NewGrmap(b.cols, b.rows)
    g.Comments = append([]string{}, b.Comments...)
    for i, p := range b.px {
        g.px[i] = uint16((int64(p.R)*2126 + int64(p.G)*7152 + int64(p.B)*722) *
            math.MaxUint16 / (math.MaxUint8 * 10000))
    }
    return g
}

// Bitmap method Grmap, converts Grmap to Bitmap.  All pixels in the resulting
// color Bitmap will be (very nearly) shades of gray.
func (g *Grmap) Bitmap() *Bitmap {
    b := NewBitmap(g.cols, g.rows)
    b.Comments = append([]string{}, g.Comments...)
    for i, p := range g.px {
        roundedSum := int(p) * 3 * math.MaxUint8 / math.MaxUint16
        rounded := uint8(roundedSum / 3)
        remainder := roundedSum % 3
        b.px[i].R = rounded
        b.px[i].G = rounded
        b.px[i].B = rounded
        if remainder > 0 {
            odd := rand.Intn(3)
            switch odd + (remainder * 3) {
            case 3:
                b.px[i].R++
            case 4:
                b.px[i].G++
            case 5:
                b.px[i].B++
            case 6:
                b.px[i].G++
                b.px[i].B++
            case 7:
                b.px[i].R++
                b.px[i].B++
            case 8:
                b.px[i].R++
                b.px[i].G++
            }
        }
    }
    return b
}
