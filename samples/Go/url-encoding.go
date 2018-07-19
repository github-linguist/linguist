package main

import (
    "fmt"
    "net/url"
)

func main() {
    fmt.Println(url.QueryEscape("http://foo bar/"))
}
