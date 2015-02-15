package main

import (
    "database/sql"
    "fmt"
    "log"

    _ "github.com/mattn/go-sqlite3"
)

func main() {
    // task req: show database connection
    db, err := sql.Open("sqlite3", "rc.db")
    if err != nil {
        log.Print(err)
        return
    }
    defer db.Close()
    // task req: create table with typed fields, including a unique id
    _, err = db.Exec(`create table addr (
        id     int unique,
        street text,
        city   text,
        state  text,
        zip    text
    )`)
    if err != nil {
        log.Print(err)
        return
    }
    // show output:  query the created field names and types
    rows, err := db.Query(`pragma table_info(addr)`)
    if err != nil {
        log.Print(err)
        return
    }
    var field, storage string
    var ignore sql.RawBytes
    for rows.Next() {
        err = rows.Scan(&ignore, &field, &storage, &ignore, &ignore, &ignore)
        if err != nil {
            log.Print(err)
            return
        }
        fmt.Println(field, storage)
    }
}
