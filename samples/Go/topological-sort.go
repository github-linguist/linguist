package main

import (
    "fmt"
    "strings"
)

var data = `
LIBRARY          LIBRARY DEPENDENCIES
=======          ====================
des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee
dw01             ieee dw01 dware gtech
dw02             ieee dw02 dware
dw03             std synopsys dware dw03 dw02 dw01 ieee gtech
dw04             dw04 ieee dw01 dware gtech
dw05             dw05 ieee dware
dw06             dw06 ieee dware
dw07             ieee dware
dware            ieee dware
gtech            ieee gtech
ramlib           std ieee
std_cell_lib     ieee std_cell_lib
synopsys         `

func main() {
    // validate that data is positioned in string as expected
    lines := strings.Split(data, "\n")
    if lines[2][0] != '=' || strings.TrimSpace(lines[len(lines)-1]) == "" {
        panic("data format")
    }
    // toss header lines
    lines = lines[3:]

    // scan and interpret input, build directed graph
    dg := make(map[string][]string)
    for _, line := range lines {
        def := strings.Fields(line)
        if len(def) == 0 {
            continue // handle blank lines
        }
        lib := def[0]   // dependant (with an a) library
        list := dg[lib] // handle additional dependencies
    scan:
        for _, pr := range def[1:] { // (pr for prerequisite)
            if pr == lib {
                continue // ignore self dependencies
            }
            for _, known := range list {
                if known == pr {
                    continue scan // ignore duplicate dependencies
                }
            }
            // build: this curious looking assignment establishess a node
            // for the prerequisite library if it doesn't already exist.
            dg[pr] = dg[pr]
            // build: add edge (dependency)
            list = append(list, pr)
        }
        // build: add or update node for dependant library
        dg[lib] = list
    }

    // topological sort on dg
    for len(dg) > 0 {
        // collect libs with no dependencies
        var zero []string
        for lib, deps := range dg {
            if len(deps) == 0 {
                zero = append(zero, lib)
                delete(dg, lib) // remove node (lib) from dg
            }
        }
        // cycle detection
        if len(zero) == 0 {
            fmt.Println("libraries with un-orderable dependencies:")
            // collect un-orderable dependencies
            cycle := make(map[string]bool)
            for _, deps := range dg {
                for _, dep := range deps {
                    cycle[dep] = true
                }
            }
            // print libs with un-orderable dependencies
            for lib, deps := range dg {
                if cycle[lib] {
                    fmt.Println(lib, deps)
                }
            }
            return
        }
        // output a set that can be processed concurrently
        fmt.Println(zero)

        // remove edges (dependencies) from dg
        for _, remove := range zero {
            for lib, deps := range dg {
                for i, dep := range deps {
                    if dep == remove {
                        copy(deps[i:], deps[i+1:])
                        dg[lib] = deps[:len(deps)-1]
                        break
                    }
                }
            }
        }
    }
}
