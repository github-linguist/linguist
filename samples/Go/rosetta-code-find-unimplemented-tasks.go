package main

import (
    "encoding/xml"
    "fmt"
    "io"
    "net/http"
    "net/url"
)

const language = "Go"

var baseQuery = "http://rosettacode.org/mw/api.php?action=query" +
    "&format=xml&list=categorymembers&cmlimit=100"

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
    // get language members, store in a map
    langMap := make(map[string]bool)
    storeLang := func(cm string) { langMap[cm] = true }
    languageQuery := baseQuery + "&cmtitle=Category:" + language
    continueAt := req(languageQuery, storeLang)
    for continueAt > "" {
        continueAt = req(languageQuery+"&cmcontinue="+continueAt, storeLang)
    }

    // a quick check to avoid long output
    if len(langMap) == 0 {
        fmt.Println("no tasks implemented for", language)
        return
    }

    // get tasks, print as we go along
    printUnImp := func(cm string) {
        if !langMap[cm] {
            fmt.Println(cm)
        }
    }
    taskQuery := baseQuery + "&cmtitle=Category:Programming_Tasks"
    continueAt = req(taskQuery, printUnImp)
    for continueAt > "" {
        continueAt = req(taskQuery+"&cmcontinue="+continueAt, printUnImp)
    }
}
