package main

import (
    "debug/elf"
    "debug/gosym"
    "fmt"
    "log"
    "math"
    "os"
    "runtime"
)

var bloop = -3.4

func main() {
    fmt.Println("task 1: verify version")
    fmt.Println("   program compiled with", runtime.Version())

    fmt.Println("task 2: check for presence of variable and function")
    // inspect ELF symbol table
    f, err := elf.Open(os.Args[0])
    if err != nil {
        log.Fatal(err)
    }
    defer f.Close()
    symSection := f.Section(".gosymtab")
    lineSection := f.Section(".gopclntab")
    textSection := f.Section(".text")
    if symSection == nil || lineSection == nil || textSection == nil {
        log.Fatal("symbolic information not found")
    }
    symData, err := symSection.Data()
    if err != nil {
        log.Fatal(err)
    }
    lineData, err := lineSection.Data()
    if err != nil {
        log.Fatal(err)
    }
    table, err := gosym.NewTable(symData,
        gosym.NewLineTable(lineData, textSection.Addr))
    if err != nil {
        log.Fatal("  ", err)
    }
    var foundBloop, foundFabs bool
    for _, s := range table.Syms {
        if s.Name == "main.bloop" {
            foundBloop = true
            fmt.Println("   bloop symbol table entry:", s)
        } else if s.Name == "math.Abs" {
            foundFabs = true
            fmt.Println("   abs symbol table entry:", s)
        }
    }
    if foundBloop && foundFabs {
        fmt.Println("   bloop:     ", bloop)
        fmt.Println("   abs(bloop): ", math.Abs(bloop))
    }
}
