package main

import (
    "container/heap"
    "image"
    "image/color"
    "image/png"
    "log"
    "math"
    "os"
    "sort"
)

func main() {
    f, err := os.Open("Quantum_frog.png")
    if err != nil {
        log.Fatal(err)
    }
    img, err := png.Decode(f)
    f.Close()
    if err != nil {
        log.Fatal(err)
    }
    fq, err := os.Create("frog256.png")
    if err != nil {
        log.Fatal(err)
    }
    err = png.Encode(fq, quant(img, 256))
    if err != nil {
        log.Fatal(err)
    }
}

// Organize quatization in some logical steps.
func quant(img image.Image, nq int) image.Image {
    qz := newQuantizer(img, nq) // set up a work space
    qz.cluster()                // cluster pixels by color
    return qz.Paletted()        // generate paletted image from clusters
}

// A workspace with members that can be accessed by methods.
type quantizer struct {
    img image.Image // original image
    cs  []cluster   // len is the desired number of colors
    px  []point     // list of all points in the image
    ch  chValues    // buffer for computing median
    eq  []point     // additional buffer used when splitting cluster
}

type cluster struct {
    px       []point // list of points in the cluster
    widestCh int     // rx, gx, bx const for channel with widest value range
    chRange  uint32  // value range (vmax-vmin) of widest channel
}

type point struct{ x, y int }
type chValues []uint32
type queue []*cluster

const (
    rx = iota
    gx
    bx
)

func newQuantizer(img image.Image, nq int) *quantizer {
    b := img.Bounds()
    npx := (b.Max.X - b.Min.X) * (b.Max.Y - b.Min.Y)
    // Create work space.
    qz := &quantizer{
        img: img,
        ch:  make(chValues, npx),
        cs:  make([]cluster, nq),
    }
    // Populate initial cluster with all pixels from image.
    c := &qz.cs[0]
    px := make([]point, npx)
    c.px = px
    i := 0
    for y := b.Min.Y; y < b.Max.Y; y++ {
        for x := b.Min.X; x < b.Max.X; x++ {
            px[i].x = x
            px[i].y = y
            i++
        }
    }
    return qz
}

func (qz *quantizer) cluster() {
    // Cluster by repeatedly splitting clusters.
    // Use a heap as priority queue for picking clusters to split.
    // The rule will be to spilt the cluster with the most pixels.
    // Terminate when the desired number of clusters has been populated
    // or when clusters cannot be further split.
    pq := new(queue)
    // Initial cluster.  populated at this point, but not analyzed.
    c := &qz.cs[0]
    for i := 1; ; {
        qz.setColorRange(c)
        // Cluster cannot be split if all pixels are the same color.
        // Only enqueue clusters that can be split.
        if c.chRange > 0 {
            heap.Push(pq, c) // add new cluster to queue
        }
        // If no clusters have any color variation, mark the end of the
        // cluster list and quit early.
        if len(*pq) == 0 {
            qz.cs = qz.cs[:i]
            break
        }
        s := heap.Pop(pq).(*cluster) // get cluster to split
        c = &qz.cs[i]                // set c to new cluster
        i++
        m := qz.Median(s)
        qz.Split(s, c, m) // split s into c and s
        // If that was the last cluster, we're done.
        if i == len(qz.cs) {
            break
        }
        qz.setColorRange(s)
        if s.chRange > 0 {
            heap.Push(pq, s) // return to queue
        }
    }
}

func (q *quantizer) setColorRange(c *cluster) {
    // Find extents of color values in each channel.
    var maxR, maxG, maxB uint32
    minR := uint32(math.MaxUint32)
    minG := uint32(math.MaxUint32)
    minB := uint32(math.MaxUint32)
    for _, p := range c.px {
        r, g, b, _ := q.img.At(p.x, p.y).RGBA()
        if r < minR {
            minR = r
        }
        if r > maxR {
            maxR = r
        }
        if g < minG {
            minG = g
        }
        if g > maxG {
            maxG = g
        }
        if b < minB {
            minB = b
        }
        if b > maxB {
            maxB = b
        }
    }
    // See which channel had the widest range.
    s := gx
    min := minG
    max := maxG
    if maxR-minR > max-min {
        s = rx
        min = minR
        max = maxR
    }
    if maxB-minB > max-min {
        s = bx
        min = minB
        max = maxB
    }
    c.widestCh = s
    c.chRange = max - min // also store the range of that channel
}

func (q *quantizer) Median(c *cluster) uint32 {
    px := c.px
    ch := q.ch[:len(px)]
    // Copy values from appropriate channel to buffer for computing median.
    switch c.widestCh {
    case rx:
        for i, p := range c.px {
            ch[i], _, _, _ = q.img.At(p.x, p.y).RGBA()
        }
    case gx:
        for i, p := range c.px {
            _, ch[i], _, _ = q.img.At(p.x, p.y).RGBA()
        }
    case bx:
        for i, p := range c.px {
            _, _, ch[i], _ = q.img.At(p.x, p.y).RGBA()
        }
    }
    // Median algorithm.
    sort.Sort(ch)
    half := len(ch) / 2
    m := ch[half]
    if len(ch)%2 == 0 {
        m = (m + ch[half-1]) / 2
    }
    return m
}

func (q *quantizer) Split(s, c *cluster, m uint32) {
    px := s.px
    var v uint32
    i := 0
    lt := 0
    gt := len(px) - 1
    eq := q.eq[:0] // reuse any existing buffer
    for i <= gt {
        // Get pixel value of appropriate channel.
        r, g, b, _ := q.img.At(px[i].x, px[i].y).RGBA()
        switch s.widestCh {
        case rx:
            v = r
        case gx:
            v = g
        case bx:
            v = b
        }
        // Categorize each pixel as either <, >, or == median.
        switch {
        case v < m:
            px[lt] = px[i]
            lt++
            i++
        case v > m:
            px[gt], px[i] = px[i], px[gt]
            gt--
        default:
            eq = append(eq, px[i])
            i++
        }
    }
    // Handle values equal to the median.
    if len(eq) > 0 {
        copy(px[lt:], eq) // move them back between the lt and gt values.
        // Then, if the number of gt values is < the number of lt values,
        // fix up i so that the split will include the eq values with
        // the gt values.
        if len(px)-i < lt {
            i = lt
        }
        q.eq = eq // squirrel away (possibly expanded) buffer for reuse
    }
    // Split the pixel list.
    s.px = px[:i]
    c.px = px[i:]
}

func (qz *quantizer) Paletted() *image.Paletted {
    cp := make(color.Palette, len(qz.cs))
    pi := image.NewPaletted(qz.img.Bounds(), cp)
    for i := range qz.cs {
        px := qz.cs[i].px
        // Average values in cluster to get palette color.
        var rsum, gsum, bsum int64
        for _, p := range px {
            r, g, b, _ := qz.img.At(p.x, p.y).RGBA()
            rsum += int64(r)
            gsum += int64(g)
            bsum += int64(b)
        }
        n64 := int64(len(px))
        cp[i] = color.NRGBA64{
            uint16(rsum / n64),
            uint16(gsum / n64),
            uint16(bsum / n64),
            0xffff,
        }
        // set image pixels
        for _, p := range px {
            pi.SetColorIndex(p.x, p.y, uint8(i))
        }
    }
    return pi
}

// Implement sort.Interface for sort in median algorithm.
func (c chValues) Len() int           { return len(c) }
func (c chValues) Less(i, j int) bool { return c[i] < c[j] }
func (c chValues) Swap(i, j int)      { c[i], c[j] = c[j], c[i] }

// Implement heap.Interface for priority queue of clusters.
func (q queue) Len() int { return len(q) }

// Less implements rule to select cluster with greatest number of pixels.
func (q queue) Less(i, j int) bool {
    return len(q[j].px) < len(q[i].px)
}

func (q queue) Swap(i, j int) {
    q[i], q[j] = q[j], q[i]
}
func (pq *queue) Push(x interface{}) {
    c := x.(*cluster)
    *pq = append(*pq, c)
}
func (pq *queue) Pop() interface{} {
    q := *pq
    n := len(q) - 1
    c := q[n]
    *pq = q[:n]
    return c
}
