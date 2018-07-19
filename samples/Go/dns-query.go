package main

import (
    "fmt"
    "net"
)

func main() {
    if addrs, err := net.LookupHost("www.kame.net"); err == nil {
        fmt.Println(addrs)
    } else {
        fmt.Println(err)
    }
}
