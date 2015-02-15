package main

import "fmt"

const (
	S = 10
)

type node struct {
	v     float64
	fixed int
}

func alloc2(w, h int) [][]node {
	a := make([][]node, h)

	for i := range a {
		a[i] = make([]node, w)
	}
	return a
}

func set_boundary(m [][]node) {
	m[1][1].fixed = 1
	m[1][1].v = 1
	m[6][7].fixed = -1
	m[6][7].v = -1
}

func calc_diff(m [][]node, d [][]node, w, h int) float64 {
	total := 0.0
	for i := 0; i < h; i++ {
		for j := 0; j < w; j++ {
			v := 0.0
			n := 0
			if i != 0 {
				v += m[i-1][j].v
				n++
			}
			if j != 0 {
				v += m[i][j-1].v
				n++
			}
			if i+1 < h {
				v += m[i+1][j].v
				n++
			}
			if j+1 < w {
				v += m[i][j+1].v
				n++
			}

			v = m[i][j].v - v/float64(n)
			d[i][j].v = v
			if m[i][j].fixed == 0 {
				total += v * v
			}
		}
	}
	return total
}

func iter(m [][]node, w, h int) float64 {
	d := alloc2(w, h)
	diff := 1.0e10
	cur := []float64{0, 0, 0}

	for diff > 1e-24 {
		set_boundary(m)
		diff = calc_diff(m, d, w, h)
		for i := 0; i < h; i++ {
			for j := 0; j < w; j++ {
				m[i][j].v -= d[i][j].v
			}
		}
	}

	for i := 0; i < h; i++ {
		for j := 0; j < w; j++ {
			t := 0
			if i != 0 {
				t += 1
			}
			if j != 0 {
				t += 1
			}
			if i < h-1 {
				t += 1
			}
			if j < w-1 {
				t += 1
			}
			cur[m[i][j].fixed+1] += d[i][j].v * float64(t)
		}
	}
	return (cur[2] - cur[0]) / 2
}

func main() {
	mesh := alloc2(S, S)
	fmt.Printf("R = %g\n", 2/iter(mesh, S, S))
}
