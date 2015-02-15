package main

import (
    "encoding/json"
    "fmt"
    "io"
    "os"
    "sort"
    "strings"
    "time"
    "unicode"
)

// Database record format.  Time stamp and name are required.
// Tags and notes are optional.
type Item struct {
    Stamp time.Time
    Name  string
    Tags  []string `json:",omitempty"`
    Notes string   `json:",omitempty"`
}

// Item implements stringer interface
func (i *Item) String() string {
    s := i.Stamp.Format(time.ANSIC) + "\n  Name:  " + i.Name
    if len(i.Tags) > 0 {
        s = fmt.Sprintf("%s\n  Tags:  %v", s, i.Tags)
    }
    if i.Notes > "" {
        s += "\n  Notes: " + i.Notes
    }
    return s
}

// collection of Items
type db []*Item

// db implements sort.Interface
func (d db) Len() int           { return len(d) }
func (d db) Swap(i, j int)      { d[i], d[j] = d[j], d[i] }
func (d db) Less(i, j int) bool { return d[i].Stamp.Before(d[j].Stamp) }

// hard coded database file name
const fn = "sdb.json"

func main() {
    if len(os.Args) == 1 {
        latest()
        return
    }
    switch os.Args[1] {
    case "add":
        add()
    case "latest":
        latest()
    case "tags":
        tags()
    case "all":
        all()
    case "help":
        help()
    default:
        usage("unrecognized command")
    }
}

func usage(err string) {
    if err > "" {
        fmt.Println(err)
    }
    fmt.Println(`usage:  sdb [command] [data]
    where command is one of add, latest, tags, all, or help.`)
}

func help() {
    usage("")
    fmt.Println(`
Commands must be in lower case.
If no command is specified, the default command is latest.

Latest prints the latest item.
All prints all items in chronological order.
Tags prints the lastest item for each tag.
Help prints this message.

Add adds data as a new record.  The format is,

  name [tags] [notes]

Name is the name of the item and is required for the add command.

Tags are optional.  A tag is a single word.
A single tag can be specified without enclosing brackets.
Multiple tags can be specified by enclosing them in square brackets.

Text remaining after tags is taken as notes.  Notes do not have to be
enclosed in quotes or brackets.  The brackets above are only showing
that notes are optional.

Quotes may be useful however--as recognized by your operating system shell
or command line--to allow entry of arbitrary text.  In particular, quotes
or escape characters may be needed to prevent the shell from trying to
interpret brackets or other special characters.

Examples:
sdb add Bookends                        // no tags, no notes
sdb add Bookends rock my favorite       // tag: rock, notes: my favorite
sdb add Bookends [rock folk]            // two tags
sdb add Bookends [] "Simon & Garfunkel" // notes, no tags
sdb add "Simon&Garfunkel [artist]"      // name: Simon&Garfunkel, tag: artist

As shown in the last example, if you use features of your shell to pass
all data as a single string, the item name and tags will still be identified
by separating whitespace.

The database is stored in JSON format in the file "sdb.json"
`)
}

// load data for read only purposes.
func load() (db, bool) {
    d, f, ok := open()
    if ok {
        f.Close()
        if len(d) == 0 {
            fmt.Println("no items")
            ok = false
        }
    }
    return d, ok
}

// open database, leave open
func open() (d db, f *os.File, ok bool) {
    var err error
    f, err = os.OpenFile(fn, os.O_RDWR|os.O_CREATE, 0666)
    if err != nil {
        fmt.Println("cant open??")
        fmt.Println(err)
        return
    }
    jd := json.NewDecoder(f)
    err = jd.Decode(&d)
    // EOF just means file was empty.  That's okay with us.
    if err != nil && err != io.EOF {
        fmt.Println(err)
        f.Close()
        return
    }
    ok = true
    return
}

// handle latest command
func latest() {
    d, ok := load()
    if !ok {
        return
    }
    sort.Sort(d)
    fmt.Println(d[len(d)-1])
}

// handle all command
func all() {
    d, ok := load()
    if !ok {
        return
    }
    sort.Sort(d)
    for _, i := range d {
        fmt.Println("-----------------------------------")
        fmt.Println(i)
    }
    fmt.Println("-----------------------------------")
}

// handle tags command
func tags() {
    d, ok := load()
    if !ok {
        return
    }
    // we have to traverse the entire list to collect tags so there
    // is no point in sorting at this point.
    // collect set of unique tags associated with latest item for each
    latest := make(map[string]*Item)
    for _, item := range d {
        for _, tag := range item.Tags {
            li, ok := latest[tag]
            if !ok || item.Stamp.After(li.Stamp) {
                latest[tag] = item
            }
        }
    }
    // invert to set of unique items, associated with subset of tags
    // for which the item is the latest.
    type itemTags struct {
        item *Item
        tags []string
    }
    inv := make(map[*Item][]string)
    for tag, item := range latest {
        inv[item] = append(inv[item], tag)
    }
    // now we sort just the items we will output
    li := make(db, len(inv))
    i := 0
    for item := range inv {
        li[i] = item
        i++
    }
    sort.Sort(li)
    // finally ready to print
    for _, item := range li {
        tags := inv[item]
        fmt.Println("-----------------------------------")
        fmt.Println("Latest item with tags", tags)
        fmt.Println(item)
    }
    fmt.Println("-----------------------------------")
}

// handle add command
func add() {
    if len(os.Args) < 3 {
        usage("add command requires data")
        return
    } else if len(os.Args) == 3 {
        add1()
    } else {
        add4()
    }
}

// add command with one data string.  look for ws as separators.
func add1() {
    data := strings.TrimLeftFunc(os.Args[2], unicode.IsSpace)
    if data == "" {
        // data must have at least some non-whitespace
        usage("invalid name")
        return
    }
    sep := strings.IndexFunc(data, unicode.IsSpace)
    if sep < 0 {
        // data consists only of a name
        addItem(data, nil, "")
        return
    }
    name := data[:sep]
    data = strings.TrimLeftFunc(data[sep:], unicode.IsSpace)
    if data == "" {
        // nevermind trailing ws, it's still only a name
        addItem(name, nil, "")
        return
    }
    if data[0] == '[' {
        sep = strings.Index(data, "]")
        if sep < 0 {
            // close bracketed list for the user.  no notes.
            addItem(name, strings.Fields(data[1:]), "")
        } else {
            // brackets make things easy
            addItem(name, strings.Fields(data[1:sep]),
                strings.TrimLeftFunc(data[sep+1:], unicode.IsSpace))
        }
        return
    }
    sep = strings.IndexFunc(data, unicode.IsSpace)
    if sep < 0 {
        // remaining word is a tag
        addItem(name, []string{data}, "")
    } else {
        // there's a tag and some data
        addItem(name, []string{data[:sep]},
            strings.TrimLeftFunc(data[sep+1:], unicode.IsSpace))
    }
}

// add command with multiple strings remaining on command line
func add4() {
    name := os.Args[2]
    tag1 := os.Args[3]
    if tag1[0] != '[' {
        // no brackets makes things easy
        addItem(name, []string{tag1}, strings.Join(os.Args[4:], " "))
        return
    }
    if tag1[len(tag1)-1] == ']' {
        // tags all in one os.Arg is easy too
        addItem(name, strings.Fields(tag1[1:len(tag1)-1]),
            strings.Join(os.Args[4:], " "))
        return
    }
    // start a list for tags
    var tags []string
    if tag1 > "[" {
        tags = []string{tag1[1:]}
    }
    for x, tag := range os.Args[4:] {
        if tag[len(tag)-1] != ']' {
            tags = append(tags, tag)
        } else {
            // found end of tag list
            if tag > "]" {
                tags = append(tags, tag[:len(tag)-1])
            }
            addItem(name, tags, strings.Join(os.Args[5+x:], " "))
            return
        }
    }
    // close bracketed list for the user.  no notes.
    addItem(name, tags, "")
}

// complete the add command
func addItem(name string, tags []string, notes string) {
    db, f, ok := open()
    if !ok {
        return
    }
    defer f.Close()
    // add the item and format JSON
    db = append(db, &Item{time.Now(), name, tags, notes})
    sort.Sort(db)
    js, err := json.MarshalIndent(db, "", "  ")
    if err != nil {
        fmt.Println(err)
        return
    }
    // time to overwrite the file
    if _, err = f.Seek(0, 0); err != nil {
        fmt.Println(err)
        return
    }
    f.Truncate(0)
    if _, err = f.Write(js); err != nil {
        fmt.Println(err)
    }
}
