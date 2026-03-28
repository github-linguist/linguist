// Krypton source file analysis — reads a file and counts lines and tokens
// Source: https://github.com/krypton-lang/krypton/blob/main/examples/srclen.k
// License: MIT

func countLines(text) {
    let i = 0
    let lines = 1
    while i < len(text) {
        if text[i] == "\n" {
            lines = lines + 1
        }
        i = i + 1
    }
    emit lines
}

func countNonBlankLines(text) {
    let i = 0
    let lines = 0
    let lineHasContent = 0
    while i < len(text) {
        let c = text[i]
        if c == "\n" {
            if lineHasContent {
                lines = lines + 1
            }
            lineHasContent = 0
        } else if c != " " && c != "\t" && c != "\r" {
            lineHasContent = 1
        }
        i = i + 1
    }
    if lineHasContent {
        lines = lines + 1
    }
    emit lines
}

func isWhitespace(c) {
    emit c == " " || c == "\n" || c == "\t" || c == "\r"
}

func countWords(text) {
    let i = 0
    let words = 0
    let inWord = 0
    while i < len(text) {
        if isWhitespace(text[i]) {
            if inWord {
                words = words + 1
                inWord = 0
            }
        } else {
            inWord = 1
        }
        i = i + 1
    }
    if inWord {
        words = words + 1
    }
    emit words
}

just run {
    let filename = arg(0)
    let source = readFile(filename)
    let srcLen = len(source)
    let lines = countLines(source)
    let nonBlank = countNonBlankLines(source)
    let words = countWords(source)

    print("File: " + filename)
    print("  Bytes:          " + srcLen)
    print("  Lines:          " + lines)
    print("  Non-blank lines:" + nonBlank)
    print("  Words:          " + words)
}
