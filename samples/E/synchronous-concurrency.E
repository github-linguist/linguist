def printer := {
    var count := 0
    def printer {
        to run(item) {
            count += 1
            println(item)
        }
        to getCount() {
            return count
        }
    }
}

def sender(lines) {
    switch (lines) {
        match [] {
            when (def count := printer <- getCount()) -> {
                println(`$count lines were printed.`)
            }
        }
        match [line] + rest {
            when (printer <- run(line)) -> {
                sender(rest)
            }
        }
    }
}

# Stream IO in E is not finished yet, so this example just uses a list.
sender(<file:input.txt>.getText().split("\n"))
