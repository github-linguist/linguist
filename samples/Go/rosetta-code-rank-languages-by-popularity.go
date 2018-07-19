package main

import (
    "encoding/xml"
    "fmt"
    "io"
    "io/ioutil"
    "net/http"
    "net/url"
    "regexp"
    "sort"
    "strconv"
    "strings"
)

var baseQuery = "http://rosettacode.org/mw/api.php?action=query" +
    "&format=xml&list=categorymembers&cmlimit=500"

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

// satisfy sort interface (reverse sorting)
type pop struct {
    string
    int
}
type popList []pop

func (pl popList) Len() int      { return len(pl) }
func (pl popList) Swap(i, j int) { pl[i], pl[j] = pl[j], pl[i] }
func (pl popList) Less(i, j int) bool {
    switch d := pl[i].int - pl[j].int; {
    case d > 0:
        return true
    case d < 0:
        return false
    }
    return pl[i].string < pl[j].string
}

func main() {
    // get languages, store in a map
    langMap := make(map[string]bool)
    storeLang := func(cm string) {
        if strings.HasPrefix(cm, "Category:") {
            cm = cm[9:]
        }
        langMap[cm] = true
    }
    languageQuery := baseQuery + "&cmtitle=Category:Programming_Languages"
    continueAt := req(languageQuery, storeLang)
    for continueAt > "" {
        continueAt = req(languageQuery+"&cmcontinue="+continueAt, storeLang)
    }
    // allocate slice for sorting
    s := make(popList, 0, len(langMap))

    // get big list of categories
    resp, err := http.Get("http://rosettacode.org/mw/index.php" +
        "?title=Special:Categories&limit=5000")
    var page []byte
    if err == nil {
        page, err = ioutil.ReadAll(resp.Body)
        resp.Body.Close()
    }
    if err != nil {
        fmt.Println(err)
        return
    }
    // split out fields of interest and populate sortable slice
    rx := regexp.MustCompile("<li><a.*>(.*)</a>.*[(]([0-9]+) member")
    for _, sm := range rx.FindAllSubmatch(page, -1) {
        ls := string(sm[1])
        if langMap[ls] {
            if n, err := strconv.Atoi(string(sm[2])); err == nil {
                s = append(s, pop{ls, n})
            }
        }
    }

    // output
    sort.Sort(s)
    for i, lang := range s {
        fmt.Printf("%3d. %3d - %s\n", i+1, lang.int, lang.string)
    }
}
