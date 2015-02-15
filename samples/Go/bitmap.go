// Raster package used with a number of RC tasks.
//
// For each task, documentation in package main source will list this
// file and others that are necessary to build a raster package with
// sufficient functionality for the task.  To build a working program,
// build a raster package from the files listed, install the package,
// and then compile and link the package main that completes the task.
//
// Alternatively, files in the raster package can be combined as desired
// to build a package that meets the needs of multiple tasks.
package raster

// Rgb is a 24 bit color value represented with a 32 bit int
// in the conventional way.  This is expected to be convenient
// for the programmer in many cases.
type Rgb int32

// Pixel has r, g, and b as separate fields.  This is used as
// the in-memory representation of a bitmap.
type Pixel struct {
    R, G, B byte
}

// Pixel returns a new Pixel from a Rgb value
func (c Rgb) Pixel() Pixel {
    return Pixel{R: byte(c >> 16), G: byte(c >> 8), B: byte(c)}
}

// Rgb returns a single Rgb value computed from rgb fields of a Pixel
// of a Pixel.
func (p Pixel) Rgb() Rgb {
    return Rgb(p.R)<<16 | Rgb(p.G)<<8 | Rgb(p.B)
}

// Bitmap is the in-memory representation, or image storage type of a bitmap.
// Zero value for type is a valid zero-size bitmap.
// The only exported field is Comments.  Remaining fields have interdepencies
// that are managed by package code and so should not be directly accessed
// from outside the package.
type Bitmap struct {
    Comments   []string
    rows, cols int
    px         []Pixel   // all pixels as a single slice, row major order
    pxRow      [][]Pixel // rows of pixels as slices of px
}

const creator = "# Creator: Rosetta Code http://rosettacode.org/"

// New is a Bitmap "constructor."  Parameters x and y are extents.
// That is, the new bitmap will have x columns and y rows.
func NewBitmap(x, y int) (b *Bitmap) {
    b = &Bitmap{
        Comments: []string{creator},
        rows:     y, // named fields here to prevent possible mix-ups.
        cols:     x,
        px:       make([]Pixel, x*y),
        pxRow:    make([][]Pixel, y),
    }
    // Note rows of pixels are not allocated separately.
    // Rather the whole bitmap is allocted in one chunk as px.
    // This simplifies allocation and maintains locality.
    x0, x1 := 0, x
    for i := range b.pxRow {
        b.pxRow[i] = b.px[x0:x1] // slice operation. does no allocation.
        x0, x1 = x1, x1+x
    }
    return b
}

// Extent returns bitmap dimensions.
func (b *Bitmap) Extent() (cols, rows int) {
    return b.cols, b.rows
}

// Fill entire bitmap with solid color.
func (b *Bitmap) Fill(p Pixel) {
    for i := range b.px {
        b.px[i] = p
    }
}

func (b *Bitmap) FillRgb(c Rgb) {
    b.Fill(c.Pixel())
}

// Set a single pixel color value.
// Clips to bitmap boundaries.
// Returns true if pixel was set, false if clipped.
func (b *Bitmap) SetPx(x, y int, p Pixel) bool {
    defer func() { recover() }()
    b.pxRow[y][x] = p
    return true
}

func (b *Bitmap) SetPxRgb(x, y int, c Rgb) bool {
    return b.SetPx(x, y, c.Pixel())
}

// Note:  Clipping to bitmap boundaries is needed for program correctness
// but is otherwise not required by the task.  It is implemented with the
// combination of pxRow and the deferred recover.  SetPx, GetPx return the
// clipping result as a way for higher level graphics functions to track
// plotting and clipping status.  As this is not required by tasks though,
// it is generally not implemented.

// Get a single pixel color value.
// Returns pixel and ok=true if coordinates are within bitmap boundaries.
// Returns ok=false if coordinates are outside bitmap boundaries.
func (b *Bitmap) GetPx(x, y int) (p Pixel, ok bool) {
    defer func() { recover() }()
    return b.pxRow[y][x], true
}

func (b *Bitmap) GetPxRgb(x, y int) (Rgb, bool) {
    p, ok := b.GetPx(x, y)
    if !ok {
        return 0, false
    }
    return p.Rgb(), true
}
