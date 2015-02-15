package main

import "fmt"

type point struct {
    x, y float32
}

var subjectPolygon = []point{{50, 150}, {200, 50}, {350, 150}, {350, 300},
    {250, 300}, {200, 250}, {150, 350}, {100, 250}, {100, 200}}

var clipPolygon = []point{{100, 100}, {300, 100}, {300, 300}, {100, 300}}

func main() {
    var cp1, cp2, s, e point
    inside := func(p point) bool {
        return (cp2.x-cp1.x)*(p.y-cp1.y) > (cp2.y-cp1.y)*(p.x-cp1.x)
    }
    intersection := func() (p point) {
        dcx, dcy := cp1.x-cp2.x, cp1.y-cp2.y
        dpx, dpy := s.x-e.x, s.y-e.y
        n1 := cp1.x*cp2.y - cp1.y*cp2.x
        n2 := s.x*e.y - s.y*e.x
        n3 := 1 / (dcx*dpy - dcy*dpx)
        p.x = (n1*dpx - n2*dcx) * n3
        p.y = (n1*dpy - n2*dcy) * n3
        return
    }
    outputList := subjectPolygon
    cp1 = clipPolygon[len(clipPolygon)-1]
    for _, cp2 = range clipPolygon { // WP clipEdge is cp1,cp2 here
        inputList := outputList
        outputList = nil
        s = inputList[len(inputList)-1]
        for _, e = range inputList {
            if inside(e) {
                if !inside(s) {
                    outputList = append(outputList, intersection())
                }
                outputList = append(outputList, e)
            } else if inside(s) {
                outputList = append(outputList, intersection())
            }
            s = e
        }
        cp1 = cp2
    }
    fmt.Println(outputList)
}
