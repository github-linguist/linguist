#!/usr/bin/env rune

def f := <file:notes.txt>
def date := makeCommand("date")

switch (interp.getArgs()) {
    match [] {
        if (f.exists()) {
            for line in f { print(line) }
        }
    }
    match noteArgs {
        def w := f.textWriter(true)
        w.print(date()[0], "\t", " ".rjoin(noteArgs), "\n")
        w.close()
    }
}
