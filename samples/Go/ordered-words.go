package main

import (
    "bytes"
    "fmt"
    "io/ioutil"
)

func main() {
    // read into memory in one chunk
    b, err := ioutil.ReadFile("unixdict.txt")
    if err != nil {
        fmt.Println(err)
        return
    }
    // split at line ends
    bss := bytes.Split(b, []byte{'\n'})

    // accumulate result
    var longest int
    var list [][]byte
    for _, bs := range bss {
        // don't bother with words shorter than
        // our current longest ordered word
        if len(bs) < longest {
            continue
        }
        // check for ordered property
        var lastLetter byte
        for i := 0; ; i++ {
            if i == len(bs) {
                // end of word.  it's an ordered word.
                // save it and break from loop
                if len(bs) > longest {
                    longest = len(bs)
                    list = list[:0]
                }
                list = append(list, bs)
                break
            }
            // check next letter
            b := bs[i]
            if b < 'a' || b > 'z' {
                continue // not a letter.  ignore.
            }
            if b < lastLetter {
                break // word not ordered.
            }
            // letter passes test
            lastLetter = b
        }
    }
    // print result
    for _, bs := range list {
        fmt.Println(string(bs))
    }
}
