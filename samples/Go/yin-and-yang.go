package main

import (
    "fmt"
    "os"
    "text/template"
)

var tmpl = `<?xml version="1.0"?>
<svg xmlns="http://www.w3.org/2000/svg"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    width="210" height="150">
<symbol id="yy" viewBox="0 0 200 200">
<circle stroke="black" stroke-width="2" fill="white"
    cx="100" cy="100" r="99" />
<path fill="black"
    d="M100 100 a49 49 0 0 0 0 -98
    v-1 a99 99 0 0 1 0 198
    v-1 a49 49 0 0 1 0 -98" />
<circle fill="black" cx="100" cy="51" r="17" />
<circle fill="white" cx="100" cy="149" r="17" />
</symbol>
{{range .}}<use xlink:href="#yy"
    x="{{.X}}" y="{{.Y}}" width="{{.Sz}}" height="{{.Sz}}"/>
{{end}}</svg>
`

// structure specifies position and size to draw symbol
type xysz struct {
    X, Y, Sz int
}

// example data to specify drawing the symbol twice,
// with different position and size.
var yys = []xysz{
    {20, 20, 100},
    {140, 30, 60},
}

func main() {
    xt := template.New("")
    template.Must(xt.Parse(tmpl))
    f, err := os.Create("yy.svg")
    if err != nil {
        fmt.Println(err)
        return
    }
    if err := xt.Execute(f, yys); err != nil {
        fmt.Println(err)
    }
    f.Close()
}
