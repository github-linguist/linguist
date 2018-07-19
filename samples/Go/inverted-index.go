package main

import (
    "bufio"
    "bytes"
    "errors"
    "fmt"
    "io"
    "os"
)

// inverted index representation
var index map[string][]int // ints index into indexed
var indexed []doc

type doc struct {
    file  string
    title string
}

func main() {
    // initialize representation
    index = make(map[string][]int)

    // build index
    if err := indexDir("docs"); err != nil {
        fmt.Println(err)
        return
    }

    // run user interface
    ui()
}

func indexDir(dir string) error {
    df, err := os.Open(dir)
    if err != nil {
        return err
    }
    fis, err := df.Readdir(-1)
    if err != nil {
        return err
    }
    if len(fis) == 0 {
        return errors.New(fmt.Sprintf("no files in %s", dir))
    }
    indexed := 0
    for _, fi := range fis {
        if !fi.IsDir() {
            if indexFile(dir + "/" + fi.Name()) {
                indexed++
            }
        }
    }
    return nil
}

func indexFile(fn string) bool {
    f, err := os.Open(fn)
    if err != nil {
        fmt.Println(err)
        return false // only false return
    }

    // register new file
    x := len(indexed)
    indexed = append(indexed, doc{fn, fn})
    pdoc := &indexed[x]

    // scan lines
    r := bufio.NewReader(f)
    lines := 0
    for {
        b, isPrefix, err := r.ReadLine()
        switch {
        case err == io.EOF:
            return true
        case err != nil:
            fmt.Println(err)
            return true
        case isPrefix:
            fmt.Printf("%s: unexpected long line\n", fn)
            return true
        case lines < 20 && bytes.HasPrefix(b, []byte("Title:")):
            // in a real program you would write code
            // to skip the Gutenberg document header
            // and not index it.
            pdoc.title = string(b[7:])
        }
        // index line of text in b
        // again, in a real program you would write a much
        // nicer word splitter.
    wordLoop:
        for _, bword := range bytes.Fields(b) {
            bword := bytes.Trim(bword, ".,-~?!\"'`;:()<>[]{}\\|/=_+*&^%$#@")
            if len(bword) > 0 {
                word := string(bword)
                dl := index[word]
                for _, d := range dl {
                    if d == x {
                        continue wordLoop
                    }
                }
                index[word] = append(dl, x)
            }
        }
    }
    return true
}

func ui() {
    fmt.Println(len(index), "words indexed in", len(indexed), "files")
    fmt.Println("enter single words to search for")
    fmt.Println("enter a blank line when done")
    var word string
    for {
        fmt.Print("search word: ")
        wc, _ := fmt.Scanln(&word)
        if wc == 0 {
            return
        }
        switch dl := index[word]; len(dl) {
        case 0:
            fmt.Println("no match")
        case 1:
            fmt.Println("one match:")
            fmt.Println("   ", indexed[dl[0]].file, indexed[dl[0]].title)
        default:
            fmt.Println(len(dl), "matches:")
            for _, d := range dl {
                fmt.Println("   ", indexed[d].file, indexed[d].title)
            }
        }
    }
}
