package main

import (
    "bytes"
    "errors"
    "fmt"
    "io/ioutil"
    "os"
)

func main() {
    if err := removeLines("foobar.txt", 1, 2); err != nil {
        fmt.Println(err)
    }
}

func removeLines(fn string, start, n int) (err error) {
    if start < 1 {
        return errors.New("invalid request.  line numbers start at 1.")
    }
    if n < 0 {
        return errors.New("invalid request.  negative number to remove.")
    }
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
    cut, ok := skip(b, start-1)
    if !ok {
        return fmt.Errorf("less than %d lines", start)
    }
    if n == 0 {
        return nil
    }
    tail, ok := skip(cut, n)
    if !ok {
        return fmt.Errorf("less than %d lines after line %d", n, start)
    }
    t := int64(len(b) - len(cut))
    if err = f.Truncate(t); err != nil {
        return
    }
    if len(tail) > 0 {
        _, err = f.WriteAt(tail, t)
    }
    return
}

func skip(b []byte, n int) ([]byte, bool) {
    for ; n > 0; n-- {
        if len(b) == 0 {
            return nil, false
        }
        x := bytes.IndexByte(b, '\n')
        if x < 0 {
            x = len(b)
        } else {
            x++
        }
        b = b[x:]
    }
    return b, true
}
