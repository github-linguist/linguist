package main

import (
    "fmt"
    "net/url"
)

const escaped = "http%3A%2F%2Ffoo%20bar%2F"

func main() {
    if u, err := url.QueryUnescape(escaped); err == nil {
        fmt.Println(u)
    } else {
        fmt.Println(err)
    }
}
