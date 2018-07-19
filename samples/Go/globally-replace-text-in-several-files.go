package main

import (
    "bytes"
    "io/ioutil"
    "log"
    "os"
)

func main() {
    gRepNFiles("Goodbye London!", "Hello New York!", []string{
        "a.txt",
        "b.txt",
        "c.txt",
    })
}

func gRepNFiles(olds, news string, files []string) {
    oldb := []byte(olds)
    newb := []byte(news)
    for _, fn := range files {
        if err := gRepFile(oldb, newb, fn); err != nil {
            log.Println(err)
        }
    }
}

func gRepFile(oldb, newb []byte, fn string) (err error) {
    var f *os.File
    if f, err = os.OpenFile(fn, os.O_RDWR, 0); err != nil {
        return
    }
    defer func() {
        if cErr := f.Close(); err == nil {
            err = cErr
        }
    }()
    var b []byte
    if b, err = ioutil.ReadAll(f); err != nil {
        return
    }
    if bytes.Index(b, oldb) < 0 {
        return
    }
    r := bytes.Replace(b, oldb, newb, -1)
    if err = f.Truncate(0); err != nil {
        return
    }
    _, err = f.WriteAt(r, 0)
    return
}
