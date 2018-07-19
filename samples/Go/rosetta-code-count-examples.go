package main

import (
    "bytes"
    "encoding/xml"
    "fmt"
    "io"
    "io/ioutil"
    "net/http"
    "net/url"
    "strings"
)

func req(u string, foundCm func(string)) string {
    resp, err := http.Get(u)
    if err != nil {
        fmt.Println(err) // connection or request fail
        return ""
    }
    defer resp.Body.Close()
    for p := xml.NewDecoder(resp.Body); ; {
        t, err := p.RawToken()
        switch s, ok := t.(xml.StartElement); {
        case err == io.EOF:
            return ""
        case err != nil:
            fmt.Println(err)
            return ""
        case !ok:
            continue
        case s.Name.Local == "cm":
            for _, a := range s.Attr {
                if a.Name.Local == "title" {
                    foundCm(a.Value)
                }
            }
        case s.Name.Local == "categorymembers" && len(s.Attr) > 0 &&
            s.Attr[0].Name.Local == "cmcontinue":
            return url.QueryEscape(s.Attr[0].Value)
        }
    }
    return ""
}

func main() {
    taskQuery := "http://rosettacode.org/mw/api.php?action=query" +
        "&format=xml&list=categorymembers&cmlimit=500" +
        "&cmtitle=Category:Programming_Tasks"
    continueAt := req(taskQuery, count)
    for continueAt > "" {
        continueAt = req(taskQuery+"&cmcontinue="+continueAt, count)
    }
    fmt.Printf("Total: %d examples.\n", total)
}

var marker = []byte("=={{header|")
var total int

func count(cm string) {
    taskFmt := "http://rosettacode.org/mw/index.php?title=%s&action=raw"
    taskEsc := url.QueryEscape(strings.Replace(cm, " ", "_", -1))
    resp, err := http.Get(fmt.Sprintf(taskFmt, taskEsc))
    var page []byte
    if err == nil {
        page, err = ioutil.ReadAll(resp.Body)
        resp.Body.Close()
    }
    if err != nil {
        fmt.Println(err)
        return
    }
    examples := bytes.Count(page, marker)
    fmt.Printf("%s: %d\n", cm, examples)
    total += examples
}
