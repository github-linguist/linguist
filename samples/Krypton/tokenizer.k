// Krypton self-hosting compiler — lexer and tokenizer
// Source: https://github.com/krypton-lang/krypton/blob/main/kompiler/compile.k
// License: MIT

// === Pair helpers (value,pos) ===

func findLastComma(s) {
    let i = len(s) - 1
    while i >= 0 {
        if s[i] == "," {
            emit i
        }
        i = i - 1
    }
    emit -1
}

func pairVal(pair) {
    let c = findLastComma(pair)
    if c < 0 {
        emit pair
    }
    emit substring(pair, 0, c)
}

func pairPos(pair) {
    let c = findLastComma(pair)
    emit toInt(substring(pair, c + 1, len(pair)))
}

// === Lexer helpers ===

func isDigit(c) {
    emit c >= "0" && c <= "9"
}

func isAlpha(c) {
    emit (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || c == "_"
}

func isAlphaNum(c) {
    emit isAlpha(c) || isDigit(c)
}

func readNumber(text, i) {
    let start = i
    while i < len(text) && isDigit(text[i]) {
        i = i + 1
    }
    emit substring(text, start, i) + "," + i
}

func readIdent(text, i) {
    let start = i
    while i < len(text) && isAlphaNum(text[i]) {
        i = i + 1
    }
    emit substring(text, start, i) + "," + i
}

func readString(text, i) {
    i = i + 1
    let start = i
    let s = ""
    while i < len(text) && text[i] != "\"" {
        if text[i] == "\\" && i + 1 < len(text) {
            if i > start {
                s = s + substring(text, start, i)
            }
            s = s + substring(text, i, i + 2)
            i = i + 2
            start = i
        }
        else {
            i = i + 1
        }
    }
    if i > start {
        s = s + substring(text, start, i)
    }
    emit s + "," + (i + 1)
}

func skipWS(text, i) {
    while i < len(text) && (text[i] == " " || text[i] == "\n" || text[i] == "\t" || text[i] == "\r") {
        i = i + 1
    }
    emit i
}

func skipComment(text, i) {
    if i + 1 < len(text) && text[i] == "/" && text[i + 1] == "/" {
        while i < len(text) && text[i] != "\n" {
            i = i + 1
        }
        emit i
    }
    else if i + 1 < len(text) && text[i] == "/" && text[i + 1] == "*" {
        i = i + 2
        while i + 1 < len(text) && (text[i] != "*" || text[i + 1] != "/") {
            i = i + 1
        }
        emit i + 2
    }
    emit i
}

func isKW(word) {
    if word == "just" { emit 1 }
    else if word == "go" { emit 1 }
    else if word == "func" { emit 1 }
    else if word == "let" { emit 1 }
    else if word == "emit" { emit 1 }
    else if word == "if" { emit 1 }
    else if word == "else" { emit 1 }
    else if word == "while" { emit 1 }
    else if word == "break" { emit 1 }
    else if word == "true" { emit 1 }
    else if word == "false" { emit 1 }
    else { emit 0 }
}

func tokenize(text) {
    let out = ""
    let i = 0

    while i < len(text) {
        i = skipWS(text, i)

        if i >= len(text) {
            break
        }

        let c = text[i]

        if c == "/" && i + 1 < len(text) && (text[i + 1] == "/" || text[i + 1] == "*") {
            i = skipComment(text, i)
        }
        else if isDigit(c) {
            let pair = readNumber(text, i)
            let num = pairVal(pair)
            i = pairPos(pair)
            out = out + "INT:" + num + "\n"
        }
        else if c == "\"" {
            let pair = readString(text, i)
            let str = pairVal(pair)
            i = pairPos(pair)
            out = out + "STR:" + str + "\n"
        }
        else if isAlpha(c) {
            let pair = readIdent(text, i)
            let id = pairVal(pair)
            i = pairPos(pair)
            if isKW(id) {
                out = out + "KW:" + id + "\n"
            }
            else {
                out = out + "ID:" + id + "\n"
            }
        }
        else if c == "+" {
            out = out + "PLUS\n"
            i = i + 1
        }
        else if c == "-" {
            if i + 1 < len(text) && text[i + 1] == ">" {
                out = out + "ARROW\n"
                i = i + 2
            }
            else {
                out = out + "MINUS\n"
                i = i + 1
            }
        }
        else if c == "=" {
            if i + 1 < len(text) && text[i + 1] == "=" {
                out = out + "EQ\n"
                i = i + 2
            }
            else {
                out = out + "ASSIGN\n"
                i = i + 1
            }
        }
        else if c == "!" {
            if i + 1 < len(text) && text[i + 1] == "=" {
                out = out + "NEQ\n"
                i = i + 2
            }
            else {
                out = out + "BANG\n"
                i = i + 1
            }
        }
        else if c == "<" {
            if i + 1 < len(text) && text[i + 1] == "=" {
                out = out + "LTE\n"
                i = i + 2
            }
            else {
                out = out + "LT\n"
                i = i + 1
            }
        }
        else if c == ">" {
            if i + 1 < len(text) && text[i + 1] == "=" {
                out = out + "GTE\n"
                i = i + 2
            }
            else {
                out = out + "GT\n"
                i = i + 1
            }
        }
        else if c == "(" {
            out = out + "LPAREN\n"
            i = i + 1
        }
        else if c == ")" {
            out = out + "RPAREN\n"
            i = i + 1
        }
        else if c == "{" {
            out = out + "LBRACE\n"
            i = i + 1
        }
        else if c == "}" {
            out = out + "RBRACE\n"
            i = i + 1
        }
        else if c == "," {
            out = out + "COMMA\n"
            i = i + 1
        }
        else if c == "%" {
            out = out + "MOD\n"
            i = i + 1
        }
        else {
            i = i + 1
        }
    }

    emit out
}

// === Entry point — tokenize a file ===

just run {
    let filename = arg(0)
    let source = readFile(filename)
    let tokens = tokenize(source)
    let ntoks = lineCount(tokens)
    print(ntoks)
}
