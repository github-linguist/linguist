package main

import (
	"bytes"
	"fmt"
	"math/rand"
	"time"
)

type Field struct {
	s    [][]bool
	w, h int
}

func NewField(w, h int) Field {
	s := make([][]bool, h)
	for i := range s {
		s[i] = make([]bool, w)
	}
	return Field{s: s, w: w, h: h}
}

func (f Field) Set(x, y int, b bool) {
	f.s[y][x] = b
}

func (f Field) Next(x, y int) bool {
	on := 0
	for i := -1; i <= 1; i++ {
		for j := -1; j <= 1; j++ {
			if f.State(x+i, y+j) && !(j == 0 && i == 0) {
				on++
			}
		}
	}
	return on == 3 || on == 2 && f.State(x, y)
}

func (f Field) State(x, y int) bool {
	for y < 0 {
		y += f.h
	}
	for x < 0 {
		x += f.w
	}
	return f.s[y%f.h][x%f.w]
}

type Life struct {
	w, h int
	a, b Field
}

func NewLife(w, h int) *Life {
	a := NewField(w, h)
	for i := 0; i < (w * h / 2); i++ {
		a.Set(rand.Intn(w), rand.Intn(h), true)
	}
	return &Life{
		a: a,
		b: NewField(w, h),
		w: w, h: h,
	}
}

func (l *Life) Step() {
	for y := 0; y < l.h; y++ {
		for x := 0; x < l.w; x++ {
			l.b.Set(x, y, l.a.Next(x, y))
		}
	}
	l.a, l.b = l.b, l.a
}

func (l *Life) String() string {
	var buf bytes.Buffer
	for y := 0; y < l.h; y++ {
		for x := 0; x < l.w; x++ {
			b := byte(' ')
			if l.a.State(x, y) {
				b = '*'
			}
			buf.WriteByte(b)
		}
		buf.WriteByte('\n')
	}
	return buf.String()
}

func main() {
	l := NewLife(80, 15)
	for i := 0; i < 300; i++ {
		l.Step()
		fmt.Print("\x0c")
		fmt.Println(l)
		time.Sleep(time.Second / 30)
	}
}
