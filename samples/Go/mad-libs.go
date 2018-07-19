package main

import (
    "bufio"
    "fmt"
    "io/ioutil"
    "log"
    "os"
    "regexp"
    "strings"
)

func main() {
    pat := regexp.MustCompile("<.+?>")
    if len(os.Args) != 2 {
        fmt.Println("usage: madlib <story template file>")
        return
    }
    b, err := ioutil.ReadFile(os.Args[1])
    if err != nil {
        log.Fatal(err)
    }
    tmpl := string(b)
    s := []string{}          // patterns in order of appearance
    m := map[string]string{} // mapping from patterns to replacements
    for _, p := range pat.FindAllString(tmpl, -1) {
        if _, ok := m[p]; !ok {
            m[p] = ""
            s = append(s, p)
        }
    }
    fmt.Println("Enter replacements:")
    br := bufio.NewReader(os.Stdin)
    for _, p := range s {
        for {
            fmt.Printf("%s: ", p[1:len(p)-1])
            r, isPre, err := br.ReadLine()
            if err != nil {
                log.Fatal(err)
            }
            if isPre {
                log.Fatal("you're not playing right. :P")
            }
            s := strings.TrimSpace(string(r))
            if s == "" {
                fmt.Println("  hmm?")
                continue
            }
            m[p] = s
            break
        }
    }
    fmt.Println("\nYour story:\n")
    fmt.Println(pat.ReplaceAllStringFunc(tmpl, func(p string) string {
        return m[p]
    }))
}
